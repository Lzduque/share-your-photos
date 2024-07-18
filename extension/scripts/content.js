// Use IIFE to avoid global scope issues
(async () => {
  // imageDB :: Map String {id :: String, url :: String, reactions :: String}
  let imageDB = {}

  // How long to wait for the DOM to update with blobs
  const domTimeoutMS = 1000

  // Synchronize the slideshow to the images every 5 seconds
  const syncIntervalMS = 5000

  // The identifier for what WhatsApp uses as a container for messages
  const rowSelector = 'div[role="row"]'

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
  const isAddedImageMutation = m => {
    return (
      m.type === 'childList' &&
        m.addedNodes.length > 0 &&
        m.addedNodes[0]?.className === '' &&
        m.addedNodes[0]?.tagName === 'DIV'
    )
  }

  // Is this the kind of mutation that affected reactions in the chat?
  const isReactionMutation = m => {
    const addedReaction = (
      m.type === 'childList' &&
        m.addedNodes.length > 0 &&
        m.addedNodes[0]?.tagName === 'BUTTON' &&
        m.addedNodes[0]?.ariaLabel.toLowerCase().startsWith('reaction')
    )

    const updatedReaction = (
      m.type === 'attributes' &&
        m.target?.tagName === 'BUTTON' &&
        m.attributeName === 'aria-label'
    )

    const removedReaction = (
      m.type === 'childList' &&
        m.removedNodes.length > 0 &&
        m.removedNodes[0]?.tagName === 'BUTTON' &&
        m.removedNodes[0]?.ariaLabel.toLowerCase().startsWith('reaction')
    )

    // Debugging lines
    addedReaction && console.log('added reaction')
    updatedReaction && console.log('updated reaction')
    removedReaction && console.log('removed reaction')

    return addedReaction || updatedReaction || removedReaction
  }

  // Get new or updated image within the row and add it to the DB
  const updateImage = rowNode => {
    const id = rowNode.firstElementChild?.getAttribute('data-id')
    if (id) {
      const images = rowNode.querySelectorAll('img[src^="blob:https://web.whatsapp.com/"]')
      if (images.length > 0) {
        const lastImage = images[images.length - 1]
        const reactions = rowNode.querySelector('button')?.ariaLabel || ''
        imageDB[id] = {
          id,
          url: lastImage.src,
          reactions,
        }
      }
    }
  }

  // Send the images to the background
  const sendImageDB = () => {
    console.log('sending imageDB:', imageDB)
    chrome.runtime.sendMessage({
      images: imageDB,
    })
  }

  const updateImages = async imageRowNodes => {
    Array.from(imageRowNodes).forEach(async rowNode => {
      // Wait for the DOM to update with blobs
      await sleep(domTimeoutMS)
      updateImage(rowNode)
    })
  }

  // Listen for messages from the popup
  chrome.runtime.onMessage.addListener(async (message, _sender, _sendResponse) => {
    let syncIntervalId = null
    if (message.action === 'startObserving') {
      // Send images on first load
      const main = await afterElementLoaded('div[id="main"]')
      // Wait extra time for the DOM to populate with rows
      await sleep(domTimeoutMS)
      const rows = main.querySelectorAll(rowSelector)
      console.log('initial rows:', rows)
      await updateImages(rows)

      // Repeatedly synchronize the slideshow to the images
      syncIntervalId = setInterval(sendImageDB, syncIntervalMS)

      const observer = new MutationObserver(mutations => {
        mutations
          .filter(isAddedImageMutation)
          .forEach(async m => {
            await updateImages(m.addedNodes)
          })

        mutations
          .filter(isReactionMutation)
          .forEach(async m => {
            await sleep(domTimeoutMS)
            await updateImage(m.target.closest(rowSelector))
          })
      })

      // Start observing the target node for configured mutations
      observer.observe(main, {
        childList: true, // Observe for the addition/removal of child nodes
        characterData: true, // Observe data changes in text nodes
        attributes: true, // Observe attribute changes
        subtree: true, // Observe descendants of the target node
      })
    } else if (message.action === 'stopObserving') {
      clearInterval(syncIntervalId);
      imageDB = {}
    }
  })
})();
