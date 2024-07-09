const handleImageExtraction = () => {
  const main = document.querySelector('div[id="main"]')

  if (main) {
    // console.log('objects: ', objects)
    chrome.runtime.sendMessage({
      content: main.innerHTML,
    })
  }
}

// Listen for messages from the popup
chrome.runtime.onMessage.addListener((request, sender, sendResponse) => {
  if (request.action === 'startObserving') {
    handleImageExtraction()

    const observer = new MutationObserver((mutations) => {
      mutations.forEach((m) => {
        // console.log('Mutation className: ', m.addedNodes[0].className)
        if (
          m.type === 'childList' &&
            m.addedNodes.length > 0 &&
            m.addedNodes[0] &&
            m.addedNodes[0].className === '' &&
            m.addedNodes[0].tagName === 'DIV'
        ) {
          setTimeout(() => {
            chrome.runtime.sendMessage({
              content: document.querySelector('div[id="main"]').innerHTML,
            })
          }, 240)
        }
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
