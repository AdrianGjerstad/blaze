import blaze

while True:
    text = input("blaze > ")
    text = text.strip()
    if text == "!exit":
        break

    result, error = blaze.run('<stdin>', text)

    if error:
        print(error.as_string())
    elif result:
        print("fetch < " + (result.__repr__()))