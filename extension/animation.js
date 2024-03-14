// Function to add a new image to the grid
function addImageToGrid(image) {
	const img = document.createElement('img')
	img.src = image
	img.alt = 'WhatsApp Image'
	document.getElementById('imageGrid').appendChild(img)
}

// Listen for messages from the background script (to receive new image URLs)
chrome.runtime.onMessage.addListener((message, sender, sendResponse) => {
	if (message.image) {
		console.log('7. message.image: ', message.image)
		addImageToGrid(message.image)
	}
})
