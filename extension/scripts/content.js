// Function to handle image extraction and sending URLs to the background script
function handleImageExtraction() {
	const images = document.querySelectorAll('img') // Adjust selector as needed
	images.forEach((img) => {
		// Send each image URL to the background script
		chrome.runtime.sendMessage({imageUrl: img.src})
	})
}

// Listen for messages from the popup
chrome.runtime.onMessage.addListener((request, sender, sendResponse) => {
	if (request.action === 'startObserving') {
		// Extract and send existing images immediately
		handleImageExtraction()

		// Create a MutationObserver to watch for added images
		const observer = new MutationObserver((mutations) => {
			mutations.forEach((mutation) => {
				mutation.addedNodes.forEach((node) => {
					// Check if the added node is an image or contains images
					if (node.nodeName === 'IMG') {
						chrome.runtime.sendMessage({imageUrl: node.src})
					} else if (node.querySelectorAll) {
						node.querySelectorAll('img').forEach((img) => {
							chrome.runtime.sendMessage({imageUrl: img.src})
						})
					}
				})
			})
		})

		// Configuration of the observer:
		const config = {childList: true, subtree: true}

		// Start observing the target node for configured mutations
		observer.observe(document.body, config)
	}
})
