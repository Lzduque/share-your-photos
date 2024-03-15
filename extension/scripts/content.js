// Function to handle rows extraction and sending them to the background script
function handleImageExtraction() {
	const main = document.querySelector('div[id="main"]')
	const objects = [...main.querySelectorAll('div[role="row"]')]

	if (objects) {
		console.log('objects: ', objects)
		objects.forEach((obj) => {
			// Send the entire div row to the haskell server
			console.log('object: ', obj)
			chrome.runtime.sendMessage({row: obj})
		})
	}
}

// Listen for messages from the popup
chrome.runtime.onMessage.addListener((request, sender, sendResponse) => {
	if (request.action === 'startObserving') {
		// Extract and send existing rows immediately
		console.log('3. Received startObserving')
		handleImageExtraction()

		// Create a MutationObserver to watch for added <div class role="row">
		const observer = new MutationObserver((mutations) => {
			mutations.forEach((mutation) => {
				if (mutation.addedNodes.length) {
					mutation.addedNodes.forEach((node) => {
						if (
							node.nodeName === 'DIV' &&
							node.getAttribute('role') === 'row'
						) {
							console.log('Mutation node: ', node)
							chrome.runtime.sendMessage({
								row: node,
							})
							// }
						}
					})
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
