// Function to handle rows extraction and sending them to the background script
function handleImageExtraction() {
	const main = document.querySelector('div[id="main"]')
	if (main) {
		// Extract image URLs directly from the DOM
		const imgElements = main.querySelectorAll('img')
		const imgUrls = Array.from(imgElements).map((img) => img.src)

		// Filter out duplicate image URLs
		const uniqueImgUrls = [...new Set(imgUrls)]

		// Send each unique image URL to the background script
		uniqueImgUrls.forEach((url) => {
			chrome.runtime.sendMessage({image: url})
		})
	}
}

// Listen for messages from the background script
chrome.runtime.onMessage.addListener((request, sender, sendResponse) => {
	if (request.action === 'startObserving') {
		// Extract and send existing rows immediately
		handleImageExtraction()

		const observer = new MutationObserver((mutations) => {
			mutations.forEach((m) => {
				if (
					m.type === 'childList' &&
					m.addedNodes.length > 0 &&
					m.addedNodes[0] &&
					m.addedNodes[0].className === '' &&
					m.addedNodes[0].tagName === 'DIV'
				) {
					setTimeout(() => {
						handleImageExtraction()
					}, 240)
				}
			})
		})

		// Configuration of the observer:
		const config = {
			childList: true, // Set to true to observe for the addition/removal of child nodes
			characterData: true, // Observe data changes in text nodes
			attributes: true, // Set to true to observe attribute changes
			subtree: true, // Set to true to observe descendants of the target node
		}

		// Start observing the target node for configured mutations
		observer.observe(document.body, config)
	}
})
