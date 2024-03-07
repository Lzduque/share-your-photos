chrome.runtime.onMessage.addListener((request, sender, sendResponse) => {
	if (request.action === 'extractImageUrl') {
		const image = document.querySelector('img') // Simplified selector for demonstration
		if (image) {
			sendResponse({imageUrl: image.src})
		} else {
			sendResponse({error: 'Image not found'})
		}
	}
})
