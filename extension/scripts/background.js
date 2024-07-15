// Keep track of the slideshow tab ID
let slideshowTabId = null

// imageDB :: Map String {id :: String, url :: String, reactions :: Integer, order :: Integer}
let imageDB = {}

const startMonitoringWhatsApp = () => {
  // Open the slideshow page in a new tab
  chrome.tabs.create(
    {url: chrome.runtime.getURL('slideshow.html')},
    (tab) => {
      slideshowTabId = tab.id
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
  console.log('message received:', message)
  if (message.action === 'startMonitoringWhatsApp') {
    startMonitoringWhatsApp()
  } else if (message.images && message.from === 'content') {
    // Receiving images from WhatsApp Web tab
    Object.values(message.images)
      .forEach(({id, url, reactions}) => {
        // If the image is already in the database, update its URL and reactions but leave it in the same order
        if (imageDB[id]) {
          imageDB[id] = {
            ...imageDB[id],
            url,
            reactions,
          }
        } else {
          imageDB[id] = {
            id,
            url,
            reactions,
            order: Object.entries(imageDB).length,
          }
        }
      })

    // Forward the images to the slideshow tab
    if (slideshowTabId !== null) {
      chrome.tabs.sendMessage(slideshowTabId, {
        images: imageDB,
        from: 'background',
      })
    }
  }
})
