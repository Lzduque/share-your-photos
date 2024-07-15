# Slideshow

## Description

A Chrome extension that watches a WhatsApp Web conversation for images and turns it into a slideshow.

## How to run

### Load the Chrome extension

1. Clone this repository your computer: `git clone https://github.com/Lzduque/share-your-photos.git`
2. Open Chrome and go to [chrome://extensions/](chrome://extensions/).
3. Enable "Developer mode" using the toggle switch at the top right.
4. Click "Load unpacked" and select the `extension` directory.

### Start monitoring WhatsApp Web

1. Go to https://web.whatsapp.com/ and log in.
2. Go to the conversation chat you want to display the images from.
3. Click on the extension icon in Chrome and press the "Start slideshow" button to start monitoring. The animation page will automatically open in a new tab after you start monitoring. All new images will also be added to the animation, one by one automatically.

## Known limitations

Images must be uploaded to the chat one at a time. When a user uploads a bunch of photos at once, WhatsApp groups them together and they can no longer be extracted in the same way.
