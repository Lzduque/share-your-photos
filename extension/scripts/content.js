// Use IIFE to avoid global scope issues
(function() {
  // imageDB :: Map String {id :: String, url :: String}
  let imageDB = {}

  // How long to wait for the DOM to update with blobs
  const domTimeoutMS = 1000

  // Synchronize the slideshow to the images every 5 seconds
  const syncIntervalMS = 5000

  // TODO: remove this
  const handleImageExtraction = () => {
    const imageNodes = document.querySelectorAll('div[id="main"] div[role="row"] img[src^="blob:https://web.whatsapp.com/"]')
    const images = Array.from(imageNodes).map(img => img.src)

    if (images.length > 0) {
      chrome.runtime.sendMessage({
        images
      })
    }
  }

  // Is this the kind of mutation that has new images in the chat?
  const goodMutation = m => {
    return (
      m.type === 'childList' &&
        m.addedNodes.length > 0 &&
        m.addedNodes[0] &&
        m.addedNodes[0].className === '' &&
        m.addedNodes[0].tagName === 'DIV'
    )
  }

  // Get new image within the node and add id to the DB
  const addNewImage = (node) => {
    const id = node.firstElementChild?.getAttribute('data-id')
    if (id) {
      const images = node.querySelectorAll('img[src^="blob:https://web.whatsapp.com/"]')
      if (images.length > 0) {
        const lastImage = {
          id,
          url: images[images.length - 1].src
        }
        imageDB[id] = lastImage
      }
    }
  }

  // Send the images to the slideshow
  const sendImageDB = () => {
    console.log('imageDB:', imageDB)
    chrome.runtime.sendMessage({
      images: imageDB
    })
  }

  // Synchronize the slideshow to the images
  setInterval(sendImageDB, syncIntervalMS)

  // Listen for messages from the popup
  chrome.runtime.onMessage.addListener((request, sender, sendResponse) => {
    if (request.action === 'startObserving') {
      // Send images on first load
      const main = document.querySelector('div[id="main"]')
      if (main) {
        const rows = main.querySelectorAll('div[role="row"]')
        Array.from(rows).forEach(row => {
          setTimeout(() => {
            addNewImage(row)
          }, domTimeoutMS)
        })
      }

      // OLD:
      // handleImageExtraction()

      const observer = new MutationObserver(mutations => {
        mutations
          .filter(goodMutation)
          .forEach(m => {
            Array.from(m.addedNodes).forEach(node => {
              // Wait for the DOM to update with blobs
              setTimeout(() => {
                addNewImage(node)
              }, domTimeoutMS)
            })
          })
      })

      // Configuration of the observer:
      const config = {
        childList: true, // Observe for the addition/removal of child nodes
        characterData: true, // Observe data changes in text nodes
        attributes: true, // Observe attribute changes
        subtree: true, // Observe descendants of the target node
      }

      // Start observing the target node for configured mutations
      observer.observe(document.body, config)
    }
  })
})();
