// Use IIFE to avoid global scope issues
(async () => {
  // imageDB :: Map String {id :: String, url :: String, reactions :: String, order :: Integer}
  let imageDB = {}
  let imagePointer = -1
  const slideshowHoldTime = 5000 // milliseconds
  const imageContainer = document.getElementById('image-container')

  // Listen for messages from the background script (to receive new images)
  chrome.runtime.onMessage.addListener((message, sender) => {
    if (message.images && !sender.tab) {
      imageDB = message.images
      console.log('received imageDB:', imageDB)
    }
  })

  const parseReactions = ariaLabel => {
    return (
      ariaLabel === '' ? ''
        : ariaLabel.startsWith('reaction ') ? `${ariaLabel.split('reaction ')[1]}`
        : ariaLabel.startsWith('Reactions ') ? `${ariaLabel.split('Reactions ')[1]}`
        : ''
    )
  }

  const displayNextImage = () => {
    console.log('displayNextImage')
    const images = Object.values(imageDB).sort((a, b) => a.order - b.order)
    if (images.length > 0) {
      const nextImage = images.find(image => image.order > imagePointer) || images[0]
      imagePointer = nextImage.order
      
      const img = document.createElement('img')
      img.src = nextImage.url

      const reactions = document.createElement('div')
      reactions.className = 'reactions'
      reactions.textContent = parseReactions(nextImage.reactions)
      
      imageContainer.innerHTML = ''
      imageContainer.appendChild(img)
      if (nextImage.reactions) {
        imageContainer.appendChild(reactions)
      }
      imageContainer.style.opacity = 1
    }
  }

  const hideImage = () => {
    imageContainer.style.opacity = 0
  }

  // Animate the slideshow
  setInterval(() => {
    hideImage()
    setTimeout(displayNextImage, 1000) // Change the image after it fades out
  }, slideshowHoldTime)

  console.log('Starting slideshow')
})();
