// Use IIFE to avoid global scope issues
(function() {
  // imageDB :: Map String {id :: String, url :: String}
  let imageDB = {}

  // How long to wait for the DOM to update with blobs
  const domTimeoutMS = 1000

  // Synchronize the slideshow to the images every 5 seconds
  const syncIntervalMS = 5000

  const afterElementLoaded = async selector => {
    while (document.querySelector(selector) === null) {
      await new Promise(resolve => requestAnimationFrame(resolve))
    }
    return document.querySelector(selector);
  };

  const sleep = async ms => {
     await new Promise(resolve => setTimeout(resolve, ms));
  }

  // Is this the kind of mutation that has new images in the chat?
  const isGoodMutation = m => {
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
    console.log('sending imageDB:', imageDB)
    chrome.runtime.sendMessage({
      images: imageDB
    })
  }

  const addImages = async imageNodes => {
    Array.from(imageNodes).forEach(async node => {
      // Wait for the DOM to update with blobs
      await sleep(domTimeoutMS)
      addNewImage(node)
    })
  }

  // Listen for messages from the popup
  chrome.runtime.onMessage.addListener(async (request, sender, sendResponse) => {
    if (request.action === 'startObserving') {
      // Send images on first load
      const main = await afterElementLoaded('div[id="main"]')
      // Wait extra time for the DOM to populate with rows
      await sleep(domTimeoutMS)
      const rows = main.querySelectorAll('div[role="row"]')
      console.log('initial rows:', rows)
      await addImages(rows)

      // Repeatedly synchronize the slideshow to the images
      setInterval(sendImageDB, syncIntervalMS)

      const observer = new MutationObserver(mutations => {
        mutations
          .filter(isGoodMutation)
          .forEach(async m => {
            await addImages(m.addedNodes)
          })
      })

      // Start observing the target node for configured mutations
      observer.observe(document.body, {
        childList: true, // Observe for the addition/removal of child nodes
        characterData: true, // Observe data changes in text nodes
        attributes: true, // Observe attribute changes
        subtree: true, // Observe descendants of the target node
      })
    }
  })
})();
