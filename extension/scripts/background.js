chrome.runtime.onMessage.addListener((message, sender, sendResponse) => {
	console.log('Message received: ', message)
	if (message.action === 'startMonitoringWhatsApp') {
		// Find the WhatsApp Web tab
		chrome.tabs.query({url: '*://web.whatsapp.com/*'}, (tabs) => {
			tabs.forEach((tab) => {
				// Inject the content script into the WhatsApp Web tab
				chrome.scripting.executeScript(
					{
						target: {tabId: tab.id},
						files: ['scripts/content.js'],
					},
					() => {
						// Once the content script is injected, send a message to start observing
						chrome.tabs.sendMessage(tab.id, {
							action: 'startObserving',
						})
					}
				)
			})
		})
	} else if (message.imageUrl) {
		console.log('Message image URL: ', message.imageUrl)
		fetch('http://localhost:3001/send-image', {
			method: 'POST',
			headers: {'Content-Type': 'application/json'},
			body: JSON.stringify({imageUrl: message.imageUrl}),
		})
			.then((response) => response.json())
			.then((data) => console.log('Data: ', data))
			.catch((error) => console.error('Error:', error))
	}
})
