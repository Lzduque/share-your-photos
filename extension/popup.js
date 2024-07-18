// Use IIFE to avoid global scope issues
(async () => {
  document.getElementById('startSlideshow').addEventListener('click', () => {
	// Send a message to the background script to start monitoring WhatsApp Web
	chrome.runtime.sendMessage({action: 'startSlideshow'})
  })
})();
