// Keep track of the animation tab ID
let animationTabId = null

chrome.runtime.onMessage.addListener((message, sender, sendResponse) => {
	console.log('Message received: ', message)
	if (message.action === 'startMonitoringWhatsApp') {
		// Open the animation page in a new tab
		console.log('1. startMonitoringWhatsApp')
		console.log('1. Message: ', message)
		chrome.tabs.create(
			{url: chrome.runtime.getURL('animation.html')},
			(tab) => {
				animationTabId = tab.id
			}
		)
		// Find the WhatsApp Web tab
		chrome.tabs.query({url: '*://web.whatsapp.com/*'}, (tabs) => {
			tabs.forEach((tab) => {
				// Inject the content script into the WhatsApp Web tab
				console.log('2. inject script')
				chrome.scripting.executeScript(
					{
						target: {tabId: tab.id},
						files: ['scripts/content.js'],
					},
					() => {
						// Once the content script is injected, send a message to start observing
						console.log('2. Send startObserving')
						chrome.tabs.sendMessage(tab.id, {
							action: 'startObserving',
						})
					}
				)
			})
		})
	} else if (message.imageUrl) {
		console.log('4. Message image URL: ', message.imageUrl)
		fetch('http://localhost:3001/send-image', {
			method: 'POST',
			headers: {'Content-Type': 'application/json'},
			body: JSON.stringify({imageUrl: message.imageUrl}),
		})
			.then((response) => response.json())
			.then((data) => {
				console.log('5. Data: ', data)
				if (data.echoedUrl) {
					const parsedImage = JSON.parse(data.echoedUrl)
					console.log('6. parsed data.echoedUrl: ', parsedImage)

					// When receiving an image URL, forward it to the animation tab
					if (parsedImage.imageUrl && animationTabId !== null) {
						console.log('7. There is Data! ')
						chrome.tabs.sendMessage(animationTabId, {
							image: parsedImage.imageUrl,
						})
					}
				}
			})
			.catch((error) => console.error('Error:', error))
	}
})
