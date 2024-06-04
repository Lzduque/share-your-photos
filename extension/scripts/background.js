// Keep track of the animation tab ID
let animationTabId = null

chrome.runtime.onMessage.addListener(async (message, sender, sendResponse) => {
	if (message.action === 'startMonitoringWhatsApp') {
		// Open the animation page in a new tab
		console.log('1. startMonitoringWhatsApp')
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
						chrome.tabs.sendMessage(
							tab.id,
							{
								action: 'startObserving',
							},
							(response) => {
								if (chrome.runtime.lastError) {
									console.error(
										chrome.runtime.lastError.message
									)
								} else {
									console.log(
										'Content script injected successfully'
									)
								}
							}
						)
					}
				)
			})
		})
	} else if (message.image) {
		if (animationTabId !== null) {
			chrome.tabs.sendMessage(animationTabId, {
				image: message.image,
			})
		}
	}
})
