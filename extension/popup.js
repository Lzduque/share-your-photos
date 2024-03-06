document.addEventListener('DOMContentLoaded', function () {
	var showImagesButton = document.getElementById('sendData')

	showImagesButton.addEventListener('click', function () {
		// Send a message to the background script to open the animation page
		chrome.runtime.sendMessage(
			{action: 'openAnimationPage'},
			function (response) {
				console.log('Response from background script:', response)
			}
		)
	})
})
