// imageDB :: Map String {order :: Integer, id :: String, url :: String}
let imageDB = {}
let imagePointer = -1
const slideshowHoldTime = 5000 // milliseconds
const imageContainer = document.getElementById('image-container')

// Listen for messages from the background script (to receive new images)
chrome.runtime.onMessage.addListener((message, sender, sendResponse) => {
  if (message.images) {
    imageDB = message.images
    console.log('received imageDB:', imageDB)
  }
})

const displayNextImage = () => {
  console.log('displayNextImage')
  const images = Object.values(imageDB).sort((a, b) => a.order - b.order)
  if (images.length > 0) {
    const nextImage = images.find(image => image.order > imagePointer) || images[0]
    imagePointer = nextImage.order
    
    const img = document.createElement('img')
    img.src = nextImage.url
    
    imageContainer.innerHTML = ''
    imageContainer.appendChild(img)
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

console.log('animation start')
