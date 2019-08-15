import blaze

while True:
    text = input("blaze > ")
    if text == "!exit":
        break

    result, error = blaze.run('<stdin>', text)

    if error:
        print(error.as_string())
    else:
        print(result)