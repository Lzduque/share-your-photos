// Use IIFE to avoid global scope issues
(async () => {
  // Keep track of the tabs
  let slideshowTabId = null
  let contentTabId = null

  // imageDB :: Map String {id :: String, url :: String, reactions :: Integer, order :: Integer}
  let imageDB = {}

  const resetImageDB = () => {
    imageDB = {}
  }

  const createSlideshow = () => {
    // Open the slideshow page in a new tab
    chrome.tabs.create(
      {url: chrome.runtime.getURL('slideshow.html')},
      tab => {
        slideshowTabId = tab.id
      }
    )
  }

  const startMonitoringWhatsApp = () => {
    // Find the WhatsApp Web tab
    chrome.tabs.query({url: '*://web.whatsapp.com/*'}, tabs => {
      if (tabs.length >= 1) {
        const tab = tabs[0]
        contentTabId = tab.id
        // Inject the content script into the WhatsApp Web tab
        chrome.scripting.executeScript({
          target: {tabId: contentTabId},
          files: ['scripts/content.js'],
        }, () => {
          // Once the content script is injected, send a message to start observing
          chrome.tabs.sendMessage(contentTabId, {
            action: 'startObserving',
          })
        })
      }
    })
  }

  chrome.tabs.onRemoved.addListener(tabId => {
    if (tabId === slideshowTabId) {
      chrome.tabs.sendMessage(contentTabId, {
        action: 'stopObserving',
      })
    }
  })

  chrome.runtime.onMessage.addListener(async (message, sender) => {
    if (message.action === 'startSlideshow') {
      resetImageDB()
      createSlideshow()
      startMonitoringWhatsApp()
    } else if (message.images && sender.tab?.title === 'WhatsApp') {
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

      // Remove any images that are present in the database but not in the received images
      Object.keys(imageDB)
        .forEach(id => {
          if (!message.images[id]) {
            delete imageDB[id]
          }
        })

      // Forward the images to the slideshow tab
      if (slideshowTabId !== null) {
        chrome.tabs.sendMessage(slideshowTabId, {
          images: imageDB,
        })
      }
    }
  })
})();
