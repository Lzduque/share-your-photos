let queue = {}
let counter = 0
let currentIndex = 0
let isGridMode = false // Track the current mode
const imageContainer = document.getElementById('image-container')
const img = document.createElement('img') // Create an img element
const imageGrid = document.createElement('div') // Create a div for the grid
imageGrid.id = 'imageGrid'
imageGrid.style.display = 'none' // Initially hide the grid
imageGrid.style.display = 'grid' // Use CSS Grid for layout
imageGrid.style.gridTemplateColumns = 'repeat(auto-fill, minmax(100px, 1fr))' // Adjust grid settings as needed
imageGrid.style.gap = '10px' // Adjust gap between images

imageContainer.appendChild(img) // Append the img element to the container
document.body.appendChild(imageGrid) // Append the grid to the body

// Function to add a new image to the grid
const addImageToGrid = (image) => {
	const img = document.createElement('img')
	img.src = image
	img.alt = 'WhatsApp Image'
	img.style.width = '100%' // Ensure the image fits within the grid cell
	img.style.height = 'auto' // Maintain aspect ratio
	imageGrid.appendChild(img)
}

// Function to display the next image in animation mode
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

// Function to hide the image
function hideImage() {
	imageContainer.style.opacity = 0 // Fade the image out before changing the source
}

// Add image to the queue
const addImageToQueue = (image) => {
	queue[counter++] = image // Use counter as the key and increment it
	console.log('New image added to queue:', image)
	if (isGridMode) {
		addImageToGrid(image) // Add the image to the grid if in grid mode
	}
}

// Listen for messages from the background script (to receive new image URLs)
chrome.runtime.onMessage.addListener((message, sender, sendResponse) => {
	if (message.image) {
		addImageToQueue(message.image)
	}
})

// Change the image every 5 seconds in animation mode
setInterval(() => {
	if (!isGridMode) {
		hideImage()
		setTimeout(displayNextImage, 1000) // Change the image after it fades out
	}
}, 5000)

// Initially display the first image
displayNextImage()

// Toggle between animation and grid modes
const toggleMode = () => {
	isGridMode = !isGridMode
	if (isGridMode) {
		imageContainer.style.display = 'none'
		imageGrid.style.display = 'grid'
		// Clear the grid before adding images to avoid duplication
		imageGrid.innerHTML = ''
		// Add all queued images to the grid
		for (let key in queue) {
			addImageToGrid(queue[key])
		}
	} else {
		imageContainer.style.display = 'block'
		imageGrid.style.display = 'none'
	}
	console.log('Mode toggled:', isGridMode ? 'Grid' : 'Animation')
}

// Add a button to toggle modes for testing
const toggleButton = document.createElement('button')
toggleButton.innerText = 'Toggle Mode'
toggleButton.style.position = 'absolute'
toggleButton.style.top = '10px'
toggleButton.style.right = '10px'
toggleButton.addEventListener('click', toggleMode)
document.body.appendChild(toggleButton)
