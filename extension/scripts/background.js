// background.js
let socket = null
const serverUrl = 'ws://localhost:3001' // Update with your actual server WebSocket URL

function connectWebSocket() {
	if (socket && socket.readyState === WebSocket.OPEN) return

	socket = new WebSocket(serverUrl)

	socket.onopen = () => {
		console.log('WebSocket connection established with the server.')
	}

	socket.onerror = (error) => {
		console.error('WebSocket error:', error)
	}

	socket.onmessage = (event) => {
		console.log('Message from server:', event.data)
		chrome.tabs.query(
			{url: chrome.runtime.getURL('animation.html')},
			(tabs) => {
				if (tabs.length > 0) {
					let animationTabId = tabs[0].id
					chrome.tabs.sendMessage(animationTabId, {
						action: 'newImages',
						data: event.data,
					})
				} else {
					console.log('Opening animation.html as it is not open.')
					chrome.tabs.create(
						{url: chrome.runtime.getURL('animation.html')},
						(tab) => {
							// Consider adding logic here to ensure animation.html is ready before sending data
						}
					)
				}
			}
		)
	}
}

chrome.runtime.onMessage.addListener((request, sender, sendResponse) => {
	console.log('Received request: ', request)
	if (request.imageData) {
		// Assuming 'imageData' is a more descriptive property name
		console.log(
			'Received image data from content script:',
			request.imageData
		)
		if (socket.readyState === WebSocket.OPEN) {
			socket.send(JSON.stringify({imageData: request.imageData}))
		} else {
			console.log('WebSocket not connected. Attempting to reconnect...')
			connectWebSocket()
			// Consider adding logic to queue messages or wait for connection
		}
	} else if (request.action === 'openAnimationPage') {
		chrome.tabs.create(
			{url: chrome.runtime.getURL('animation.html')},
			() => {
				sendResponse({status: 'Animation page opened'})
				// Consider additional signaling from animation.html to indicate readiness
			}
		)
		return true // Indicates that sendResponse will be called asynchronously
	}
})

connectWebSocket() // Initiate WebSocket connection
