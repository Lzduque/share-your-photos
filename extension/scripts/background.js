// Keep track of the animation tab ID
let animationTabId = null

// imageDB :: Map String {order :: Integer, id :: String, url :: String}
let imageDB = {}

const startMonitoringWhatsApp = () => {
  // Open the animation page in a new tab
  chrome.tabs.create(
    {url: chrome.runtime.getURL('animation.html')},
    (tab) => {
      animationTabId = tab.id
    }
  )
  // Find the WhatsApp Web tab
  chrome.tabs.query({url: '*://web.whatsapp.com/*'}, (tabs) => {
    if (tabs.length >= 1) {
      const tab = tabs[0]
      // Inject the content script into the WhatsApp Web tab
      chrome.scripting.executeScript({
        target: {tabId: tab.id},
        files: ['scripts/content.js'],
      }, () => {
        // Once the content script is injected, send a message to start observing
        chrome.tabs.sendMessage(tab.id, {
          action: 'startObserving',
        })
      })
    }
  })
}

chrome.runtime.onMessage.addListener(async (message, _sender, _sendResponse) => {
  if (message.action === 'startMonitoringWhatsApp') {
    startMonitoringWhatsApp()
  } else if (message.images) {
    // Receiving images from WhatsApp Web tab
    Object.values(message.images)
      .forEach(({id, url}) => {
        // If the image is already in the database, update its URL but leave it in the same order
        if (imageDB[id]) {
          imageDB[id].url = url
        } else {
          imageDB[id] = {
            order: Object.entries(imageDB).length,
            id,
            url,
          }
        }
      })

    // Forward the images to the animation tab
    if (animationTabId !== null) {
      chrome.tabs.sendMessage(animationTabId, {
        images: imageDB,
      })
    }
  }
})
