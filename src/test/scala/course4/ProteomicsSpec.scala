package course4

import org.scalatest.FlatSpec

class ProteomicsSpec extends FlatSpec {

  import course4.Proteomics._

  it should "return Graph spectrum" in {
    val input = "57 71 154 185 301 332 415 429 486"
      .split(" ").map(_.toInt).toList
    val result = graphSpectrum(input)
    val expected = List(
        (154,301),
        (57,154),
        (332,429),
        (429,486),
        (185,332),
        (0,71),
        (71,185),
        (301,429),
        (415,486),
        (301,415),
        (57,185),
        (0,57)
    )
    assert(expected === result)
  }

  it should "return Peptide for ideal spectrum" in {
    val input = "57 71 154 185 301 332 415 429 486"
      .split(" ").map(_.toInt).toList
    val expected = "GPFNA"
    val result = decodingIdealSpectrum(input)
    assert(expected === result)
  }

  it should "return correct Peptide Vector" in {
    val integer_mass : Map[String, Int] = Map("X" -> 4, "Z" -> 5)
    val peptide = "XZZXX"
    val result =  getPeptideVector(peptide)(integer_mass)
    val expected = "0 0 0 1 0 0 0 0 1 0 0 0 0 1 0 0 0 1 0 0 0 1"
    assert(result === expected)
  }

  it should "return decode peptide from Peptide Vector" in {
    val vector = "0 0 0 1 0 0 0 0 1 0 0 0 0 1 0 0 0 1 0 0 0 1"
    val integer_mass_revers = Map(4 -> "X", 5 -> "Z" )
    val result = peptideVectorToPeptide(vector)(integer_mass_revers)
    val expected = "XZZXX"
    assert(result === expected)
  }

  it should "return correct peptideIdentification value" in {
    val spectralVector = "0 0 0 4 -2 -3 -1 -7 6 5 3 2 1 9 3 -8 0 3 1 2 1 8"
        .split(" ").map(_.toInt).toList
    val proteome = "XZZXZXXXZXZZXZXXZ"
    val integer_mass : Map[String, Int] = Map("X" -> 4, "Z" -> 5)
    val result = peptideIdentification(spectralVector, proteome)(integer_mass)
    val expected = "ZXZXX"
    assert(expected === result._1)
  }

  it should "return correct peptideIdentification(Other Alphabet) value" in {
    val spectralVector = "-20 -17 -20 6 5 -10 -16 10 -1 -5 21 28 14 18 1 23 12 25 -17 5 -15 21 2 24 24 -11 21 18 -9 4 24 -2 -6 -1 6 -18 14 -8 -5 11 -6 7 -10 2 29 -1 30 19 -18 21 -2 -3 23 -10 14 6 10 -4 -15 13 18 -12 28 9 -7 -7 -12 3 10 -12 7 25 29 -8 -19 -4 -15 5 4 -5 17 26 2 -17 -5 -1 -13 7 -9 16 -11 -10 -6 23 9 27 -1 17 -15 -8 8 18 -14 5 0 18 29 -18 24 11 30 -7 17 30 14 -3 25 -14 12 -14 29 14 30 -4 -15 3 23 -8 -9 22 23 22 2 -1 2 20 16 5 2 5 19 19 -18 -5 -2 18 -1 27 18 12 23 29 -15 12 23 -2 -16 29 -16 -20 -20 -19 2 7 27 24 3 14 3 30 -19 25 25 -16 -8 17 26 -6 -20 -6 -16 -7 2 27 -10 14 20 26 23 26 -17 -4 23 8 18 -8 16 4 21 16 10 30 -17 7 -8 -5 -13 5 14 11 6 -3 -16 -19 11 -1 -7 -2 -9 2 24 27 -13 -17 29 16 1 20 30 20 -19 -9 13 17 22 -3 6 14 27 -19 15 18 20 -1 -18 18 -13 18 16 8 18 -9 8 28 -2 3 -16 -3 9 -4 -9 1 17 -5 0 -19 13 10 -13 24 -20 -3 -11 7 19 8 -1 26 20 -17 -3 24 17 28 3 -8 22 7 23 18 1 -5 -1 -7 -12 -5 -8 29 -8 -9 11 -20 9 19 -6 2 3 23 -20 -3 -7 -3 21 15 10 18 27 3 3 -16 10 30 26 15 1 15 9 24 25 -1 -5 7 -13 -10 18 -6 -2 27 -8 -8 11 -20 6 13 14 21 -12 -9 -17 20 10 12 13 10 -19 9 -14 -4 11 -1 12 0 -12 10 8 19 -10 9 15 17 -7 15 3 18 -17 14 8 27 4 18 8 22 -10 23 13 -20 -19 6 27 17 1 -17 7 -10 -16 24 28 -7 9 -7 -9 -6 -4 28 -5 15 -16 -9 -14 -17 -2 20 -12 15 -16 -7 17 28 26 -7 23 -2 25 4 24 -11 12 20 0 26 -7 4 30 28 -14 21 -16 -9 12 9 13 5 19 -10 -11 -20 24 -1 -5 4 -2 29 28 -17 -3 17 -9 -20 -18 28 -4 5 -11 17 8 -11 24 -12 -4 15 23 0 -9 2 7 14 24 1 5 6 -15 -14 11 24 -11 2 23 2 -10 30 27 -18 13 12 15 15 10 -12 -17 -20 -6 -15 -18 7 29 8 -11 7 12 5 3 7 -20 12 5 5 -5 -16 4 26 14 -19 8 11 28 16 14 16 -3 -15 -16 -2 28 10 -9 26 4 23 -19 7 -12 13 12 28 -20 24 26 -14 -4 -17 9 17 -18 -14 -5 15 -16 7 -11 10 1 11 27 7 15 -19 1 9 -12 1 -12 30 7 22 -12 20 9 23 24 28 26 19 -7 11 -20 -17 21 26 22 10 -14 15 20 -1 19 25 -18 -18 5 24 -2 20 -13 -20 -8 -4 -3 -18 1 14 13 -16 21 -15 -15 3 1 -8 1 10 10 -16 8 -11 15 -13 12 28 22 -10 -5 -16 -18 25 21 -9 18 22 -5 3 -18 -4 9 15 -6 28 27 13 4 29 17 -13 4 15 8 13 16 -4 -1 21 29 -15 30 20 28 0 11 -5 13 -14 11 30 23 21 23 22 -9 26 25 -8 24 25 -13 -5 1 -12 2 -20 29 -12 15 -8 29 24 -16 13 24 17 5 22 12 -1 20 28 -11 -14 -8 22 21 3 -17 13 4 17 -10 -5 -10 -20 -5 15 -4 6 23 30 17 3 14 15 -3 -11 -6 15 -19 4 -10 -6 -2 7 9 14 -7 -4 -18 -11 -11 26 -6 27 28 28 -11 -6 10 25 -16 -20 -17 9 29 -12 -4 30 6 -9 -1 7 10 28 23 -13 28 -1 26 -16 -4 -2 12 4 -5 0 28 -14 -17 -1 -19 1 0 3 -12 -20 21 22 14 3 5 10 12 -3 13 17 -6 29 18 -12 22 15 8 -3 -6 12 -17 -13 6 9 22 8 14 23 -5 -16 0 30 11 6 29 18 11 12 -11 -14 9 3 -2 15 7 18 -2 4 16 29 24 -17 2 -19 10 9 17 10 25 13 30 29 -10 21 4 1 30 28 7 15 23 2 -17 4 -14 -7 15 -19 9 21 -4 6 11 1 29 21 27 18 21 -4 -1 -8 -20 26 -9 24 15 10 16 27 -18 30 9 27 16 -18 -5 -1 21 -3 6 -19 21 30 17 6 -13 -20 -1 -2 8 16 -18 3 2 3 5 18 -10 22 -3 12 -7 -20 27 5 19 -7 28 5 1 9 -9 25 -10 -8 28 -9 18 12 16 24 28 -10 -8 26 -15 7 15 16 14 -4 -5 24 28 28 9 11 29 -3 -16 -7 21 15 -18 -11 17 -20 2 -1 -18 22 -18 3 22 30 10 29 25 2 -7 -3 14 -9 26 17 -10 27 20 -1 5 27 29 4 29 -12 11 18 17 -20 -14 25 -5 -9 15 15 29 -14 5 11 -17 18 -5 -5 -10 -15 20 26 -9 -15 12 6 28 7 16 -11 -20 9 -11 -17 -9 -11 15 17 27 16 -16 -5 -15 23 27 5 3 1 30 -13 -14 1 30 -19 3 21 -12 11 -2 17 -2 9 -6 -17 -15 23 27 -11 0 11 -18 -9 -15 -16 8 10 -18 -13 18 -8 12 -12 -15 14 -20 5 1 -9 6 -1 -13 -1 18 22 -12 -9 13 16 -18 6 -18 24 6 7 2 -3 22 -15 -3 -13 8 20 11 18 8 28 -5 -10 -4 0 2 20 4 1 7 -5 26 20 -3 11 5 29 -15 28 5 1 6 -18 -18 7 30 -4 14 25 -16 20 -13 4 -3 -19 -9 6 22 10 29 8 7 19 23 23 24 23 -10 17 -14 7 -19 -3 24 29 -8 8 -9 -18 -20 14 25 20 22 17 21 18 23 20 -17 0 9 -11 15 27 28 -10 -9 -15 -17 6 -19 6 18 11 9 -1 12 -6 29 21 0 11 3 7 -14 4 -1 17 10 -8 27 20 -2 0 2 28 -6 27 5 17 15 3 21 29 25 27 -8 19 19 24 0 -7 21 26 25 -14 -6 -17 -1 8 -10 -12 28 1 -3 12 -7 23 12 10 0 13 27 10 28 -9 -1 26 -17 -19 18 -15 2 20 26 -6 10 26 20 -15 16 -18 -14 -3 0 1 -3 22 -15 16 17 10 9 2 -18 9 15 -5 0 -14 6 14 26 -5 -19 -7 -1 -8 22 -20 -11 0 -19 21 -16 -10 18 25 25 24 3 24 -18 7 -3 1 13 -3 1 -9 -9 2 -20 10 11 4 28 27 -9 2 20 3 16 15 10 7 13 -10 18 26 14 22 9 12 -7 25 -9 17 11 -18 -16 21 5 -19 -3 3 -8 18 -18 25 -4 29 21 28 -14 -10 21 21 23 14 -5 23 3 21 30 23 9 30 -20 3 -20 2 19 -3 -10 20 20 4 21 4 -1 23 9 16 12 26 6 -5 20 21 -9 18 -17 -10 4 -6 12 -11 -8 2 0 -16 2 -16 -16 20 -7 8 -11 -19 10 16 -15 20 10 22 10 -5 -5 2 4 4 -19 -14 4 8 14 -20 -15 -11 -9 21 24 16 7 -10 15 17 30 -10 10 17 11 17 10 7 7 -12 8 -3 26 -14 23 -11 3 -2 26 -16 21 11 10 -2 -1 -13 -17 -6 28 -7 19 29 7 -9 27 16 25 3 25 13 9 -18 0 21 -15 11 1 8 30 11 -16 16 -15 18 29 16 -5 10 -17 10 -17 -2 12 28 4 26 13 -6 -5 25 -9 -17 23 14 -8 -1 -15 -3 18 -3 -17 -1 13 -19 -1 22 30 -5 13 -6 21 19 29 9 23 14 -5 15 28 -13 -4 -15 -5 5 -14 0 -20 -1 24 -13 14 26 -11 29 21 5 5 -19 -7 3 -2 -17 24 4 24 18 14 4 25 20 21 -3 -12 6 -4 18 8 -18 1 0 13 12 20 21 20 26 24 16 25 -17 14 -10 20 -3 24 -13 9 28 2 -16 21 4 -15 6 18 10 16 3 20 -2 -13 21 0 -14 -11 -19 2 24 -1 3 5 5 1 -1 -4 -20 -15 -9 -4 26 28 21 28 17 17 9 1 3 30 0 26 4 -13 22 -3 29 18 -8 1 6 -13 -11 3 -12 8 -19 28 17 2 -19 22 -5 -19 -18 25 29 -15 12 30 25 16 22 -9 17 11 7 18 11 -5 18 -9 1 12 14 -8 -11 -19 13 3 5 11 -17 21 24 -18 -3 -7 13 29 27 19 22 16 10 -9 -8 21 -8"
      .split(" ").map(_.toInt).toList
    val proteome = "AEEDGACLIFSRKCWQKFMHLTMQDCDYAICHIRVEGPTECQGACVLLLATKFVGPWEIPRTWKWTFRKGSVFRCDAPEEDQPLTFVYKRYMLQFLSWITGNKYYGRCCKSSVRMVKWSEDKDHKYPINWRGYAWHENPSMVRNGNANQKCHGMHPNHFWKSHNICEHKVYHAGCGNCWACVVSDWVHHHQNFVWEYFVFDQKDFDRHKDITYLPFQMNMAPFHFINFVVHWMGDSIHKFNYNVWLMRNKKGYCIWNKPDWLKDKKGRQMCACHNGYFIAAKPHLHKEKPNKRMDGWAQQRARFHIPCYWTMSMGYEQCNMCKQLGEMLSICCEFVGNESWSQFCWWCRWTTRQCHMQGSDRRGMKTKLDDQCVSPACMQFCHICWYQYKNFHNDKEDYAWSCTTGHTYQDLRFMMNMPEERYPYVSSFICWGIMNMSLRHQWLKKLRGVVQEVWSTNMKPTMFQLGVWPPHGVTVCIMQVMDQSTFIFDKCPIKQRQFVYMGSTSNLLEPSWIWDNFCIGADHNKGATCIMVNMICIECAWDNGIKWGEMPRMDVHDWHGKQWVIMYFPKKMAGVWMLENFTLIYWLQSITMDIARCGFWNVQASINAYDDQEQYFWCNFCIWCQIRFMFITTFVPNVHSTPHEGQFCDPQHNACFTKRKLREKPQHFMRKNANFYTQLVIYFVSVSEMNVWYPLRMWDALDFQTITMMYMFQACTHLVDHTPDVVSGSFFWWGDTQFMPRCENFCASNVRTTYFEMATPHAKYPHHWFQMIVCWSYWQVCHSQGQSSRRKELPQWQNKTARCHRRTWITMCCCVSFLKQWCKFTYAVRRCKICMINNPVRHWSRPRDDRAALDIYFRPQWTIRLCQILNRWRYAQQRYEYSSTITVDGSHKDETTYYNKYNLDIHHKLMKIPTPNPDPDRSISRCTRAANTWHAKKYVQQIRIMHTMCCTGPWHYYHFGACIGQHTLDTFFWARNCTEWKYKQENIHDSPCNDDPTLNIPEPWCWHGRVQDTMPMKGAFMVQNAKLMANQVEHNGQNGLTTWIWGQIAANVFWSWMSLKAQGCLPYARKKANYPQMWQVVMNHMQKSMRPDIAHANGVEGEIETDIGIASNCHWYQTVNPHFMFWMYFRRTCYIFITALASWGTYIYPHETDGSNFYNACQWEACTYNAVHMDYMNERSRACYHFDMHECHFRDAQGAATANSPQQKGNRRFFKFICRGMPCYSYLTQGPACEFEEDGDWDWYGAWWCRFYEYMKILKKCCEENMNYCGTTPAGIEIIVTMNAKNCRQSYSFTRQWSIQDPAQGTVLQMPLHMSTFGLCLSHSPLMYVVNRLDGYSNHNMVWIFSIYIVSYLKPFCSNYWPDRKGIQSDIAWDDQAVYTGCSHSERWGQHSELEINTVQGDQTGNKMTQYILYSLKFVLEMQCDVVELCQNQMGDTIRNDPHETTGKVDCRRPPPRCVFNWKRFYYLFFHIVGGYSKEVMMVIETPREYYPAMMYKENFYNYTMTNYTPEYFCTDDTCIAMHHFAYGPAVRDWSEGEHIFHLMKNPMSAWEWYVSADYDDFHTSGYKNWHYALKTECRLWTMPWGKVQCAGFHDENWNIPMKQRKHECFYGNQVHAQNYNGTSNIYEYYGKIIYLWSKQFCSSDGMHYEIVRSIHMPWDLLKDIAGVRLMVDIGTYEQYLGDTKTAEAFDTDYPVQTDELGCGHFFYGRRMRMAEQFKLVRCTHQDFQTLLYTMISFQCRASWQERHFVKYIVNGIAHDNDMVVVSEARWCLIFGDVSMWYVLALDSSPQNDNYPFVYWKNSDLWHHDVALSYDVLCFVLERFHDNLNQMPWVGCMCWRDCDFFQDPQGTMLWENPKMQQSPAYQMFKRLFWYIGERQKCSWYSEARNNCIAEYFEACASGWITQAAECPSWSFTSFQFEDYLQLKMATRKYFHNYWWWWMCLPVDLQYNWSMAPPDQEFLFMCPMRHFWVPWCPVYTCQVSYDMICRYEWGVCQDLMMCGINYDQQQMYQANIWIVCTLVVYYRQGGFFMNWGWKNFEWFPIDRLANQTQCHVFNNGFHENTLMKPSTKHKRHDCTDLNQSGASREMMSLRQIKYWVSKIEVWPNWSVPPHQYDQDSYFMAKTYNAKHLYHRCQVMRLGCFGFIIPYWPQYKVGRMHVYSLLINYHAAVQGFDDNMSDHYKPKFQRKHGDSINQRNGEQWGYLGFIHNYTWVIDWCRFDETEVLPVGCQPNTHSHQVDEIAWHHKSMTPYHRHTFQGPYYMISQSARFYKQMPPGFHRRNCEGWFYKWYPMSFDNKYSHYWHGKVGVHLEQLSYHVCIWADGMKPQMQEPVHMEYPKQHMIFVKGLPLKWEWRLYLDPWSVMWVFQDKNGVARRVTFYQCAGPYRQCWKHSKKHMNDVNGSGGRAQVRVFMMTTYWHEIEDCDNLMTDCKRHVHHFSNCMYTGVHEMEVKYADGPLWQTVMPTYIQCNPYRSGCEWAFQICNPPSQPFCMYNSLHGRSKFLNAWSWSWGPMYEESYVARRYIMVLRSYGKLHAHAGSFNCQNPLSFVQHERSFCYRNCDFHNEDTADIRLIWFTSPQHYARVILKWSAKYENEPDGSAVREVQFDKVKTYRAMHWQYTQKLMMEIHIFGYGAHFPQWDGAVEACGEPRKDRWLCDDYLNPGNCLQVQADIWLQVCIMPNSITANHMGSMGQFCNAHRMVFWMMVITDMCTNARHELFCFYVHWTQACYRYGPECGWNRMNSFSVKPYNDVWIEGHGEVQQKRNGADIKENNYLMVICMNSDMTEVPPQSAQQIWYREEFTKIMVNRSICYPDGEMGFPDAHNMVWCMYQIHSSIHRYVNQPRGYKHSVTIFTFECTSLKTITNHTNQRECRIKKPSSFVCFGCEMHNTTLNFAGYTYMLEQCNKYSGPWDEGIAVWGQVNRVFIIMSMGCWVCDERRFASTPSREWHKDIYFKRVEDPAGCSRVFKMWNIPVVESCECQDGVGDSEKHECHIPKKPIYKCMYAELTDEMINHGRHRSFCIWTAFEGLQREKCLFAFPSASSKRKSLCDTMNVRFSIVNSISLNDCHHPNPVMLRINRFQQTAYVYQVWAGLVDYANEQTVIWPEWVHSTVFLHIPPDCAMSNDHQRREYVTIHECENEKEMWDCYLRRKYCLMWKWVWPFMAWIMGLEGYQEYSLVYTQPSRMFIDNEMNLFESNAVNFYLHMNHCTGHNNLHAEKCNIEVNDTRSCYNEKRFRFTASSSYQGGDKSRYHDTQLHYYENVHEMATRHVMIVVNYCCKNGYKDYYFEDKVYLPDQGNGVFVNHHECRAIAIWELMQMIAPWQPAAQSSCIVRFLPVDLDKCVGRFHTVGTWRDPCFIVCNMILNTEGDDMVYADCAYIRRNWAKMWACITWLDDFMPHAHDNLNGCVDMPQLQHSRVIGMMDYDGMYPGNHAWDALADEACYCCTKQWSKMHPCCLASYKWWASGNFCEYRPGFFGWQMCEDKCHTCTITVYRHQEYPCWSSELSWMQDRMLSRNHNSYANSQRAGECQASCPMHSFHIFSPLFKFYARLIYMSFTFWRLPIMCPMDFMQVSKNTECCVKHCVKQYEFLTRVGTSNNACFTYIKIFSFARMQNVEDKIDLKEFCHPYKNHHTEMKHMIYWTLHKRDYPRTLELWTLHREMSMRDHRAYDPPVAHAKAMDGGLTHSQCCLMTVHMIRAKCWKGYWHKVWIKNICTGNCICQCYVKYCGTNKSTVQVQEGVKKFVPEHFEHCMWNDVKIFWNDIMNMSQLPTVIQPCQLKNFIAPPEFSRQQHANQSGPTWHDSYTEMIWLNNANNIYWHDFQVNARQFWYPQIYPVWLIDYRFCSNAMKFPLKTWPQKNFLRFQICKMGTCTQYNSQNWGCNDKIHVMLLETTLVTVRQNDFDFIRCIPACFVGRECKTYLHSNICFERWKWRTLSLANAWSKRIYLDDGHQLKISNLSILREAGAWVKEWRLFWVWVDDSPSRELLLENLTIVMPPKIKASESIASILFLHPNWHSMMDQDWGWFGSDLMFVIENRHEHDWGWPVVVEMHYWQPMKQEQSLEKILGEQGQAWWFHYSYIHKMIQSRHYEFPVQVTTWSSSGMFISRKRRPVRHMVVSMTWNYEYAGRSAYLYDLMLADNDTRPAPSMQSPENTITTTTMFIDMYSYGQPPKLYPSDIFQDCRKTKFLIAKFEPVGEFGKAKWCMRGSMFKIVNSGFDHISVGWPRACMGEPYLNILDTREGFRMWGADKRRAPCEGWVTGMWDFFPIVLRGMQHVPRKMAENPERTIAAVFGWERLCITYNEHTQTDENPDGVHKPTYNSKQPFVVVMMLNNHGLDRCSCHLHPCMDCGQNHEILRVYTHECCHVPAWGKFNAPVWNKAPGLFHQAQKRAKLRLDLKWWACMMFICHWAKSDMCKMEYRTCNYEIFESTQMLLCEQRNDGSMHFYEFHYLCLDNVHGQDVFINSPIGVFANCQWMDDCYLHNKWIEGYLKAHSWESKEGPIINEGKEDTECPMMLHMHACEMGVYYIPFEQWDMCCKHWQCMAVDYFMMEQNTALLRTQMFSAFAYCFCVKIFYIYNAFKGSDWDSYMKKKYLANNPPHKGTFSNVVHIMRNALMDTLLKVEGYMETNNVLEVYSTDGKAETDGNQFVYFNWKMWHETAILHFYHGMPVYWCMHNWDIHSDPQPGRTHAAISYDACLAIETHQKSSWFRAHHLTHYLHACRTDCTINPRTSDNLWKDWADPQQRRFNAHQVTSCWMSLSSGENNGVPNWDSSQETHDDDMPEVFMEGEHAGAIGCWYQPCHNDDHLQIDNHLFATGAVGWERKAQPVALDCWKFVFYWWPLIWRREHNTGTRGGDYIHIFWGHYLGVRWTSIPDMKYCTHKTGRKQWWRFGRVWNKTKNNIYEASCKWEGTVVWTCGQNEGPYNFDVSAITCHVKGSRTQMEFHFLNYWHKMPFRNKEIPYKRTQCCCMRMSCLTWSWSEGLYWDQAFFNWVAYMNHR"
    val result = peptideIdentification(spectralVector, proteome)(integer_mass_table_revers)
    val expected = "WRYAQQRYEYSST"
    assert(expected === result._1)
  }

}
