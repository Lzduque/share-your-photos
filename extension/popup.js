document.getElementById('sendUrl').addEventListener('click', () => {
	// Send a message to the background script to start monitoring WhatsApp Web
	console.log('Clicked!')
	chrome.runtime.sendMessage({action: 'startMonitoringWhatsApp'})
})
