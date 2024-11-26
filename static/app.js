document.getElementById('joinBtn').addEventListener('click', function() {
    const username = document.getElementById('username').value.trim();
    const room = document.getElementById('room').value.trim();

    if (username === "" || room === "") {
        alert("Please enter both username and room name.");
        return;
    }

    // const ws = new WebSocket(`ws://${window.location.host}/`);
    const ws = new WebSocket("ws://172.23.69.13:8080/");  //Wireless LAN adapter Wi-Fi: iPv4 address

    ws.onopen = function() {
        // Send a join message
        const joinMsg = {
            type: "join",
            room: room,
            user: username
        };
        ws.send(JSON.stringify(joinMsg));
    
        // Display the chat UI
        document.getElementById('login').style.display = 'none';
        document.getElementById('chat').style.display = 'block';
        document.getElementById('roomName').innerText = room;
    
        // Ensure the chat container exists
        if (!document.getElementById('chat-container')) {
            const chatContainer = document.createElement('div');
            chatContainer.id = 'chat-container';
            document.getElementById('chat').appendChild(chatContainer);
        }
    };    

    // Create a message queue
    const messageQueue = [];
    ws.onmessage = function(event) {
        console.log("Message received:", event.data);

        try {
            const data = JSON.parse(event.data);

            if (data.serverUser && data.serverMessage) {
                const messageElement = document.createElement('p');
                messageElement.textContent = `${data.serverUser}: ${data.serverMessage}`;

                const chatContainer = document.getElementById('chat-container');
                if (chatContainer) {
                    // Append the message if the container exists
                    chatContainer.appendChild(messageElement);

                    // Process any queued messages
                    while (messageQueue.length > 0) {
                        const queuedMessage = messageQueue.shift();
                        chatContainer.appendChild(queuedMessage);
                    }
                } else {
                    // Queue the message if the container is not available
                    console.error("Chat container not found. Queuing message.");
                    messageQueue.push(messageElement);
                }
            } else {
                console.error("Received message with unexpected format:", data);
            }
        } catch (error) {
            console.log("Non-JSON message received:", event.data);
        }
    };

    ws.onclose = function() {
        alert("Disconnected from the server.");
        document.getElementById('login').style.display = 'block';
        document.getElementById('chat').style.display = 'none';
    };

    document.getElementById('sendBtn').addEventListener('click', function() {
        const message = document.getElementById('messageInput').value.trim();
        if (message === "") return;
        const chatMsg = {
            type: "message",
            room: room,
            user: username,
            message: message
        };
        ws.send(JSON.stringify(chatMsg));
        document.getElementById('messageInput').value = "";
    });
});
