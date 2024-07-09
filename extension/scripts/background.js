// Keep track of the animation tab ID
let animationTabId = null

// images :: [{id :: String, blobUrl :: String}]
let images = []

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
      console.log('2. inject script')
      chrome.scripting.executeScript({
        target: {tabId: tab.id},
        files: ['scripts/content.js'],
      }, () => {
        // Once the content script is injected, send a message to start observing
        console.log('2. Send startObserving')
        chrome.tabs.sendMessage(tab.id, {
          action: 'startObserving',
        })
      })
    }
  })
}

chrome.runtime.onMessage.addListener(async (message, _sender, _sendResponse) => {
  if (message.action === 'startMonitoringWhatsApp') {
    console.log('1. startMonitoringWhatsApp')
    console.log('1. Message: ', message)
    startMonitoringWhatsApp()
  } else if (message.images) {
    // Receiving HTML content from WhatsApp Web tab
    
    // TODO: parse out all images
    console.log('message.content:', message.content)

    // TODO: add new images to current list of images

    // OLD
    if (false) {
      const body = JSON.stringify({content: message.content})
      console.log('4. Message body sent: ', body)

      try {
        console.log('Fetching...')
        const response = await fetch('http://localhost:3001/send-image', {
          method: 'POST',
          headers: {'Content-Type': 'application/json'},
          body: body,
        })
        const data = await response.json()
        console.log('5. Data: ', data)
        // When receiving an image URL, forward it to the animation tab
        if (data.imageUrls && animationTabId !== null) {
          const newImages = data.imageUrls
          console.log('7. newImages: ', newImages)
          // console.log('7. animationTabId: ', animationTabId)
          newImages.map((e) =>
            chrome.tabs.sendMessage(animationTabId, {
              image: e,
            })
          )
        }
      } catch (err) {
        console.log('Error: ', err)
        console.error(err)
      }
    }
  }
})
