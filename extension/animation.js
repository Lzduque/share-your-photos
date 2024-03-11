// Function to add a new image to the grid
function addImageToGrid(imageUrl) {
	const img = document.createElement('img')
	img.src = imageUrl
	img.alt = 'WhatsApp Image'
	document.getElementById('imageGrid').appendChild(img)
}

// Listen for messages from the background script (to receive new image URLs)
chrome.runtime.onMessage.addListener((message, sender, sendResponse) => {
	if (message.imageUrl) {
		console.log('7. message.imageUrl: ', message.imageUrl)
		addImageToGrid(message.imageUrl)
	}
})
