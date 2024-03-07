chrome.runtime.onMessage.addListener((message, sender, sendResponse) => {
	console.log('Message received: ', message)
	if (message.imageUrl) {
		console.log('Message image URL: ', message.imageUrl)
		// Replace 'http://localhost:3001' with your Haskell server's actual address
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
