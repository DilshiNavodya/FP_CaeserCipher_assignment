object CeasarCipher{

  val alphabet_U = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  val alphabet_L = "abcdefghijklmnopqrstuvwxyz"

  def encrypt(key: Int, text: String)=text.map {
    case c if alphabet_U.contains(c)=> rot(alphabet_U,c,key)
    case c if alphabet_L.contains(c)=> rot(alphabet_L,c,key)
    case c => c
  }

  def decrypt(key:Int, text:String)= encrypt(-key,text)

  private def rot(i:IndexedSeq[Char], c:Char, key:Int): Char ={
    return i((c-i.head+key+i.size)%i.size)
  }
}
object caeser{
  def main(args: Array[String]){
    val text="Functional Programming"
    val shift = 3
    println("Plaintext  => " + text)
    val encoded= CeasarCipher.encrypt(shift,text)
    println("Ciphertext => " + encoded)
    println("Decrypted  => " + CeasarCipher.decrypt(shift,encoded))
  }
}
