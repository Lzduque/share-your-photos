document.getElementById('sendUrl').addEventListener('click', () => {
	// Query the active tab
	chrome.tabs.query({active: true, currentWindow: true}, (tabs) => {
		// Signal the content script to start observing changes
		chrome.tabs.sendMessage(tabs[0].id, {action: 'startObserving'})
	})
})
