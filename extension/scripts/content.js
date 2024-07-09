// Use IIFE to avoid global scope issues
(function() {
  let imageDB = {}

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
          src: images[images.length - 1].src
        }
        imageDB[id] = lastImage
        console.log(imageDB)
      }
    }
  }

  // Listen for messages from the popup
  chrome.runtime.onMessage.addListener((request, sender, sendResponse) => {
    if (request.action === 'startObserving') {
      // TODO: Don't forget to send images on first load

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
              }, 1000)
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
