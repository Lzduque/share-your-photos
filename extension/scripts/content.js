// Use IIFE to avoid global scope issues
(async () => {
  // imageDB :: Map String {id :: String, url :: String, reactions :: String}
  let imageDB = {}
  let syncIntervalId = null
  let observer = null

  // How long to wait for the DOM to update with blobs
  const domTimeoutMS = 1000

  // Synchronize the slideshow to the images every 2 seconds
  const syncIntervalMS = 2000

  // The identifier for what WhatsApp uses as a container for messages
  const rowSelector = 'div[role="row"]'

  const afterElementLoaded = async selector => {
    while (document.querySelector(selector) === null) {
      await new Promise(resolve => requestAnimationFrame(resolve))
    }
    return document.querySelector(selector)
  }

  const sleep = async ms => {
     await new Promise(resolve => setTimeout(resolve, ms))
  }

  // Is this the kind of mutation that has new images in the chat?
  const isAddedImageMutation = m => {
    return (
      m.type === 'childList' &&
        m.addedNodes.length === 1 &&
        m.addedNodes[0]?.className === '' &&
        m.addedNodes[0]?.tagName === 'DIV' &&
        m.addedNodes[0]?.role === 'row'
    )
  }

  // Is this the kind of mutation that removed an image from the chat?
  const isRemovedImageMutation = m => {
    return (
      m.type === 'childList' &&
        m.removedNodes.length === 1 &&
        m.removedNodes[0]?.className === '' &&
        m.removedNodes[0]?.tagName === 'DIV' &&
        m.removedNodes[0]?.role === 'row'
    )
  }

  // Is this the kind of mutation that affected reactions in the chat?
  const isReactionMutation = m => {
    const addedReaction = (
      m.type === 'childList' &&
        m.addedNodes.length === 1 &&
        m.addedNodes[0]?.tagName === 'BUTTON' &&
        m.addedNodes[0]?.ariaLabel.toLowerCase().startsWith('reaction')
    )

    const updatedReaction = (
      m.type === 'attributes' &&
        m.attributeName === 'aria-label' &&
        m.target?.tagName === 'BUTTON' &&
        m.target?.ariaLabel.toLowerCase().startsWith('reaction')
    )

    const removedReaction = (
      m.type === 'childList' &&
        m.removedNodes.length === 1 &&
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
  const upsertImage = rowNode => {
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

  // Remove an image from the DB
  const removeImage = rowNode => {
    const id = rowNode.firstElementChild?.getAttribute('data-id')
    if (id) {
      delete imageDB[id]
    }
  }

  // Send the images to the background
  const sendImageDB = () => {
    console.log('sending imageDB:', imageDB)
    chrome.runtime.sendMessage({
      images: imageDB,
    })
  }

  const upsertImages = async imageRowNodes => {
    // Wait for the DOM to update with blobs
    await sleep(domTimeoutMS)
    Array.from(imageRowNodes).forEach(upsertImage)
  }

  chrome.runtime.onMessage.addListener(async message => {
    if (message.action === 'startObserving') {
      // Send images on first load
      const main = await afterElementLoaded('#main div[role="application"]')
      // Wait extra time for the DOM to populate with rows
      await sleep(domTimeoutMS)
      const rows = main.querySelectorAll(rowSelector)
      console.log('initial rows:', rows)
      await upsertImages(rows)

      // Repeatedly synchronize the slideshow to the images
      syncIntervalId = setInterval(sendImageDB, syncIntervalMS)

      observer = new MutationObserver(mutations => {
        mutations
          .filter(isAddedImageMutation)
          .forEach(async m => {
            // Wait for the DOM to update with blobs
            await sleep(domTimeoutMS)
            upsertImage(m.addedNodes[0])
          })

        mutations
          .filter(isRemovedImageMutation)
          .forEach(async m => {
            await removeImage(m.removedNodes[0])
          })

        mutations
          .filter(isReactionMutation)
          .forEach(async m => {
            // Wait for the DOM to update
            await sleep(domTimeoutMS)
            await upsertImage(m.target.closest(rowSelector))
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
      console.log('Stopping')
      clearInterval(syncIntervalId)
      observer.disconnect()
      imageDB = {}
    }
  })
})();
