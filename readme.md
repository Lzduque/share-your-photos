# What I Want

I want to make a Google Chrome extension + Haskell server that can: the extension would read the online WhatsApp page and the specific conversation we are on and automatically get all the new content including image URLs and the number of reactions to each image. That data would be sent to the back end, filtering all the information, getting only the image URLs and the number of reactions on each, and sending it back to the Google Chrome extension. The Google Chrome extension, receiving the first answer, would open a custom extension page where all the images would be displayed as an animation, like a screen saver, and that will be the tab on focus for the whole time. Every time new content is posted on the WhatsApp conversation (on the background tab, not on focus), it should automatically be sent to the backend, and then the front end should receive the clean data to display. There has to be a control or some queue for the Google Chrome extension to add to the animation queue only the new images. This would also enable the back end to update the number of votes for each image and send the most up-to-date information to the extension. At the end of the presentation, we will click on a discrete button at the bottom right of the animation page and that will display the top 3 most voted images so far (up-to-date). First, it will be the title "3rd place" on the top, the image covering most of the screen and the number of votes on the bottom of the page, then scrolling down the same thing for second place and finally scrolling down again, the same thing for first place.

# Google Chrome Extension

## Content Script

-   Functionality: Monitors the WhatsApp Web page for new content in a specific conversation, including image URLs and reactions.
-   Implementation: Use MutationObservers to detect changes in the DOM related to new messages and reactions. Extract relevant data and send it to the background script.

## Background Script

-   Functionality: Manages communication between the content script, the Haskell server, and the custom extension page. Handles data processing and tab management.
-   Implementation: Receives data from the content script, sends it to the Haskell server for processing, and receives the filtered data. Manages the custom extension page tab, ensuring it remains focused and updates with new data.

## Custom Extension Page (animation.html)

-   Functionality: Displays images in an animation, updates with new images and reaction counts, and shows the top 3 images upon request.
-   Implementation: Uses JavaScript to animate images, listen for new data from the background script, and update the display accordingly. Implements a UI element for displaying top-voted images.

# Haskell Server

-   Functionality: Receives raw data from the extension, filters it to extract image URLs and reaction counts, and maintains an up-to-date record of reactions for each image.
-   Implementation: Sets up an API endpoint to receive data, processes the data to extract necessary information, and stores it, updating reaction counts as new data arrives. Sends filtered data back to the extension.

# Data Flow and Interaction

-   Content Script: Detects new messages and reactions in WhatsApp Web, sending this data to the background script.
-   Background Script: Sends raw data to the Haskell server and manages the custom extension page. It ensures the extension page is focused and updates it with new data.
-   Haskell Server: Processes incoming data, filtering for image URLs and reaction counts, and sends this filtered data back to the extension. It also updates reaction counts for existing images.
-   Custom Extension Page: Receives and displays images in an animation. Upon completion, displays top-voted images as requested.

# Additional Considerations

-   WhatsApp Web Integration: Carefully review WhatsApp's terms of service to ensure compliance. The extension's functionality, especially regarding data extraction, might be subject to limitations or restrictions.
-   Privacy and Security: Implement robust data handling practices to protect user privacy, especially when processing message content from WhatsApp Web.
-   Performance: Optimize the extension and server for performance, minimizing the impact on the user's browser and server load, particularly when monitoring for new content and processing data in real time.
-   User Experience: Design the extension's UI and the custom page to be user-friendly, ensuring clear navigation and interaction, especially for displaying top-voted images.

# Development Steps

-   Prototype: Start with a basic version of the extension and server, focusing on core functionality like detecting new messages, sending data to the server, and displaying images.
-   Iterate: Gradually add features like reaction count processing, dynamic updates to the animation page, and the top-voted images display.
-   Test: Continuously test the extension and server, both individually and as an integrated system, to identify and resolve issues.
-   Optimize: Based on testing, refine the performance and user experience, optimizing data processing, animation display, and user interaction elements.

Given the complexity of this project, it's essential to approach it in manageable stages, validating each component before integrating them into a cohesive system.
