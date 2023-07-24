object Cipher {
    def encrypt(Text: String, shift: Int): String = {
        Text.map {char =>
            if (char.isLetter){
                val base = if (char.isUpper) 'A' else 'a'
                val encrypted = (base + (char - base + shift) % 26).toChar
                encrypted
                }else {
                    char
            }
        }
    }

    def decrypt(cipherText: String, shift: Int): String = {
        encrypt(cipherText, -shift)
    }

    def main(args: Array[String]): Unit = {
        println("Enter the text to cipher:")
        val Text = scala.io.StdIn.readLine()

        println("Enter the shift value:")
        val shift = scala.io.StdIn.readInt()

        println("Do you want encrypt or decrypt(Type 'encrypt' or 'decrypt')")
        val operation = scala.io.StdIn.readLine()

        val result = operation.toLowerCase match {
        case "encrypt" => println("Encrypted: " +encrypt(Text, shift))
        case "decrypt" => println("Decrypted: " +decrypt(Text, shift))
        case _ => throw new IllegalArgumentException("Invalid operation. Use 'encrypt' or 'decrypt'.")
        }
        
    }
}  