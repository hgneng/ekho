import socket

# 创建一个TCP/IP套接字
sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

# 连接服务器地址和端口号
server_address = ('localhost', 20491)
sock.connect(server_address)

try:
    message = '我挥一挥衣袖，不带走一片云彩。'  # 要发送给服务器的消息
    print('发送数据:', message)
    sock.sendall(message.encode())  # 发送数据给服务器
    data = sock.recv(1024)  # 从服务器接收数据，最多接收1024字节
    print('收到数据:', data.decode())  # 解码接收到的数据并打印出来
finally:
    # 清理连接
    sock.close()
