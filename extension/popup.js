document.getElementById('sendData').addEventListener('click', () => {
	console.log('Button clicked!') // Check if the event is triggered
	chrome.tabs.query({active: true, currentWindow: true}, function (tabs) {
		var activeTab = tabs[0]
		console.log('Active tab:', activeTab.url) // Confirm the active tab URL is captured

		fetch('http://localhost:3000/data', {
			method: 'POST',
			headers: {
				'Content-Type': 'application/json',
			},
			body: JSON.stringify({url: activeTab.url}),
		})
			.then((response) => response.json())
			.then((data) => {
				console.log('Success:', data)
			})
			.catch((error) => {
				console.error('Error:', error)
			})
	})

	chrome.tabs.create({url: chrome.runtime.getURL('animation.html')})
})
