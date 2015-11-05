# XOR decryption
"""
Each character on a computer is assigned a unique code and 
the preferred standard is ASCII (American Standard Code for
Information Interchange). For example, uppercase A = 65, 
asterisk (*) = 42, and lowercase k = 107.

A modern encryption method is to take a text file, convert 
the bytes to ASCII, then XOR each byte with a given value,
taken from a secret key. The advantage with the XOR function 
is that using the same encryption key on the cipher text,
restores the plain text; for example, 65 XOR 42 = 107, then 107 XOR 42 = 65.

For unbreakable encryption, the key is the same length as 
the plain text message, and the key is made up of random
bytes. The user would keep the encrypted message and 
the encryption key in different locations, and without both
"halves", it is impossible to decrypt the message.

Unfortunately, this method is impractical for most users, 
so the modified method is to use a password as a key. If the
password is shorter than the message, which is likely, 
the key is repeated cyclically throughout the message. The
balance for this method is using a sufficiently long 
password key for security, but short enough to be memorable.

Your task has been made easy, as the encryption key consists of 
three lower case characters. Using cipher.txt (right
click and 'Save Link/Target As...'), a file containing the 
encrypted ASCII codes, and the knowledge that the plain text
must contain common English words, decrypt the message and 
find the sum of the ASCII values in the original text.
"""


def main():
    with open("p059_cipher.txt", 'r') as f:
        msg_int = [int(s) for s in f.read().split(',')]
    # for char in msg_int: print((lambda s: str(s) if len(str(s)) == 2 else '0' + str(s))(char), end=' ')
    key_dict = {0: [], 1: [], 2: []}
    for index in range(len(msg_int)):
        if 65 < msg_int[index] < 90:
            key_dict[index % 3].append(msg_int[index])
    # for pos in key_dict: print(key_dict[pos])

    # for i in range(97, 123):
    #     for j in range(97, 123):
    #         for k in range(97, 123):
    #             key = [i, j, k]
    #             msg = ''
    #             for index in range(len(msg_int)):
    #                 msg += chr(msg_int[index] ^ key[index % 3])
    #             # if msg.find('euler') > 0:
    #             print(msg)

    key = [103, 111, 100]
    # print(''.join(chr(i) for i in key))
    msg = ''
    ascii_sum = 0
    for index in range(len(msg_int)):
        msg += chr(msg_int[index] ^ key[index % 3])
        ascii_sum += msg_int[index] ^ key[index % 3]
    print(msg)
    print(ascii_sum)

if __name__ == "__main__":
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
