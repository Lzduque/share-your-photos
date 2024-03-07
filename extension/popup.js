document.getElementById('sendUrl').addEventListener('click', () => {
	// Query the active tab
	chrome.tabs.query({active: true, currentWindow: true}, (tabs) => {
		// Send a message to the content script
		chrome.tabs.sendMessage(
			tabs[0].id,
			{action: 'extractImageUrl'},
			(response) => {
				if (response.imageUrl) {
					// Now that we have the image URL, send it to the background script
					chrome.runtime.sendMessage({imageUrl: response.imageUrl})
				} else {
					console.error(
						'Could not extract the image URL:',
						response.error
					)
				}
			}
		)
	})
})
