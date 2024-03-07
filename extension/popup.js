document.getElementById('sendUrl').addEventListener('click', () => {
	console.log('Clicked!')
	chrome.runtime.sendMessage({
		imageUrl:
			'blob:https://web.whatsapp.com/15dc465b-e510-498a-8747-5a05ffc5a1ba',
	})
})
