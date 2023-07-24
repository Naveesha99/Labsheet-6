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
        val Text = "This Means WAR"
        val shift = 1

        println("Encrypted: "+encrypt(Text, shift))

        println("Decrypt: "+decrypt(encrypt(Text, shift), shift))
    }
}