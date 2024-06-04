// Function to add a new image to the grid
const addImageToGrid = (image) => {
	const img = document.createElement('img')
	img.src = image
	img.alt = 'WhatsApp Image'
	document.getElementById('imageGrid').appendChild(img)
}

// {Int: String}
// Example: { 1: "blob:https://web.whatsapp.com/36b0440c-9e6a-439d-b918-eac16c5896ec", 2: "..." }
// This object will hold the images, using integers as keys.
let queue = {}
let counter = 0
let currentIndex = 0
const imageContainer = document.getElementById('image-container')
const img = document.createElement('img') // Create an img element

imageContainer.appendChild(img) // Append the img element to the container

function displayNextImage() {
	if (Object.keys(queue).length > 0) {
		// Check if there are any images in the queue
		img.src = queue[currentIndex] // Set the src of the img element
		console.log('Image displayed:', img.src)

		imageContainer.style.opacity = 1 // Make the image visible

		// Move to the next image, looping back to the start if at the end
		currentIndex = (currentIndex + 1) % Object.keys(queue).length
	}
}

function hideImage() {
	imageContainer.style.opacity = 0 // Fade the image out before changing the source
}

// Add image to the queue
const addImageToQueue = (image) => {
	queue[counter++] = image // Use counter as the key and increment it
	console.log('New image added to queue:', image)
}

// Listen for messages from the background script (to receive new image URLs)
chrome.runtime.onMessage.addListener((message, sender, sendResponse) => {
	if (message.image) {
		// addImageToGrid(message.image);
		addImageToQueue(message.image)
	}
})

// Change the image every 5 seconds
setInterval(() => {
	hideImage()
	setTimeout(displayNextImage, 1000) // Change the image after it fades out
}, 5000)

// Initially display the first image
displayNextImage()
