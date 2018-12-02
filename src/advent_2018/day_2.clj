(ns advent-2018.day-2
  (:require [clojure.string :as str]))

(def data "ohvflkatysoimjxbunazgwcdpr\nohoflkctysmiqjxbufezgwcdpr\nohvflkatysciqwxfunezgwcdpr\nfhvflyatysmiqjxbunazgwcdpr\nohvhlkatysmiqjxbunhzgwcdxr\nohvflbatykmiqjxbunezgscdpr\nohvflkatasaiqjxbbnezgwcdpr\nohvflkatyymiqjxrunetgwcdpr\nohvflkatbsmiqhxbunezgwcdpw\noheflkytysmiqjxbuntzgwcdpr\nohvflkatrsmiqjibunezgwcupr\nohvflkaiysmiqjxbunkzgwkdpr\nohvilkutysmiqjxbuoezgwcdpr\nphvflkatysmkqjxbulezgwcdpr\nohvflkatnsmiqjxbznezgpcdpr\nohvylkatysriqjobunezgwcdpr\nohvflkatytmiqjxbunezrwcypr\nohvonkatysmiqjxbunezgwxdpr\nohvflkatgsmoqjxyunezgwcdpr\nohvflkbtqsmicjxbunezgwcdpr\nohvflkatysmgqjqbunezgwcdvr\nohvtlkatyrmiqjxbunezgwcdpi\nohvflkatyskovjxbunezgwcdpr\nohvflkayysmipjxbunezgwcdpu\nohvalkltysmiqjxbunezgecdpr\nohvflkatysmiqjxiunezgnndpr\nohvflkatyomiqjxbbnezgwcdpp\nohvflkatysmiqjxbuoezgncdpy\nomvflkvtysmiqjxwunezgwcdpr\nohvflkatynmicjxbunezgwpdpr\nohvflkatyqmaqjxbunezvwcdpr\nohbfhkatysmiqjxbunezgwcdqr\nohvflkatesmiqjvbunezpwcdpr\nohvflkatysmsqjxiunezgwcdhr\nohvfjkatysmwqjxbunezgwcddr\nohvflkanysmiqjxbunwkgwcdpr\nohqflkatysmiqjxbuuezgwcddr\nohvflkatysmvqjxbznlzgwcdpr\nohvflkatysmiqjxbunjzwwqdpr\nohvfjkatysmiqxxbunezgwcupr\nchvfxkatysmiqjxxunezgwcdpr\nuhvflkatitmiqjxbunezgwcdpr\nohvflbatysmiqjxbuntzgwcdor\nohvflkmtysmmqjxbunexgwcdpr\nohvflsatysmyqjxjunezgwcdpr\nohvfskatysmiqjjbunezgwcdpg\nohvflkatysniqjxbunexgwcrpr\nohvfekatysmiqjxbunedswcdpr\nohvfltatysmjqjxbunezghcdpr\nohvflkatydmiqjxvunezggcdpr\noavflkatysmiqjxtunazgwcdpr\nohvflkltysmiqjxbuzeugwcdpr\nohbflkatysmiqjybuuezgwcdpr\nehvfzkatysmiqjxbuhezgwcdpr\nodvflkatssmiqjxbunezgwcdpj\nohvflkatysmiqjzbufezgwbdpr\njhvflkdtysmiqqxbunezgwcdpr\nohvflkatysmiqjwbunengwcnpr\nohvfskatysmiqjxbxuezgwcdpr\nohvflkatysmiqjobvnezgwcrpr\nohvrlkatysmiqjxbwnezgrcdpr\nofvflkatysmiqjxbunezpwcdwr\nohvfxdatyomiqjxbunezgwcdpr\nyhvflkatydmiqjxbubezgwcdpr\nohvflkatysdiqjxbuneztwcspr\nohvflkatydmiquxbunezgwcbpr\nohvflkatysmiqcxbukezgwcdwr\nohvflkntasmiqjxbunezghcdpr\nlhvflkatysmiqjxbunezqwckpr\nehifikatysmiqjxbunezgwcdpr\nohvflkatysmiqjcbutezgwcdpm\nohvflkatjssiqrxbunezgwcdpr\noyvflkavysmiqjxlunezgwcdpr\norvflkgtysmiqjxbukezgwcdpr\nihvflkatysmiqaxbunpzgwcdpr\nohvflkatusmiqjxbbnezgwchpr\nohvflkatysbiqjxvuneugwcdpr\nohvflkatysmiqjcbungzgwcwpr\novvflkatysmidjxbunezgscdpr\nohvflqatysmiljxbunfzgwcdpr\nghvfokatysmiqjxbunqzgwcdpr\nnxvflkatysmxqjxbunezgwcdpr\nohvflkatysmiqjxbexezgwrdpr\nohvfrkatysmhqjxbuntzgwcdpr\nohvflkvtysmiqjxocnezgwcdpr\nohvglkgtysmiqjxnunezgwcdpr\nohvflkatysmnqjxbunecgwqdpr\noyvflkatysgiqjxbcnezgwcdpr\nofvflkatysmiqjxbunfzgwcdpg\notvflkttysmiqjxbunezgwmdpr\nohvflkvtysmiqjbbunezgzcdpr\nahvflkatysyiqjxbunezvwcdpr\nohiflkatysmydjxbunezgwcdpr\nohvfwkatysmvqjxbunezwwcdpr\nohvflkatysbiqjxbunergwodpr\nhhvsdkatysmiqjxbunezgwcdpr\nihvflkwtysmiqjxbunezgacdpr\nohvfljatysmiqcxbunuzgwcdpr\nohvflkatysqiqlwbunezgwcdpr\nohvflkauysmkqjxwunezgwcdpr\nohvflkatysmoqjqbunezgwodpr\nohvslkvtysmipjxbunezgwcdpr\nolvflkatysmiujxbunezgwctpr\nosvflxatysmiqjxbenezgwcdpr\norvflkhtysmiqjxbinezgwcdpr\nohcflkatystiqjxbunezbwcdpr\nohcflkatyfmifjxbunezgwcdpr\nohvflkatdsmiqjxbrnezgwcdpt\nohvflkatysmiqjxbwnqzawcdpr\noevflkakysmiqjxbunezgwcdpt\nofvflkatysmiqjxbunbqgwcdpr\nohvflkatysmdqjxbunefqwcdpr\nohvklkalysmiqjxbunezgwcepr\nocvflhatysmiqjxbunezzwcdpr\nuhvflkatysmiqmxbunezgwcxpr\nohvflkatyshikjhbunezgwcdpr\nlbvflkatysmoqjxbunezgwcdpr\nohvflkatssmuqjxbunezgscdpr\nohvflkatysmifyxbuvezgwcdpr\nohvfikatysmiqjxbunezgwfupr\nohvmlkaiysmiqjxqunezgwcdpr\nohvflkatysmiqjxiunpzgwcdpo\nlhvflkatysmpqjxbenezgwcdpr\nohvflkatysmiqjobunengwczpr\nohoflkatysniqjxbunezgccdpr\nohvfxkatysmiqjgbunyzgwcdpr\nohvflkytysmiljxbubezgwcdpr\nhhvsdkatysmiqjxjunezgwcdpr\nohvflkatysmiqjtuunezgwcdpt\nohvfdkxtysmiqjubunezgwcdpr\nohxflkatysmiyjxbunezgwcdhr\nohvflkatysmiqjibunezgwcppd\nohvflkatysmihjxbunezgwcdhj\nohvflkatysmiqjxronezgwcdvr\nofrflxatysmiqjxbunezgwcdpr\nohvwlkatysmiqjxounezgscdpr\nohvflkatcodiqjxbunezgwcdpr\noqvflkatysmiqjxbunebgwmdpr\nohvflmatysmisjxbunezqwcdpr\novvflkatysmiqjxbuxezgwcdpe\nohvflkatysmdejxbuneztwcdpr\nhhvflkathsmiqjxbwnezgwcdpr\nohkflkatlsmsqjxbunezgwcdpr\nohvflkktysmizjxhunezgwcdpr\nohzflkatysmiqjrbunezgwcdpj\nohuflwatysmiqjxbunezgwcdgr\nohvflkatysmiqvxmunpzgwcdpr\nxhvflkwtysmiqjxbunezgwjdpr\nwhvflkatysmiqjxbunezgzcopr\nohvflkayysmiqjxuznezgwcdpr\nkhvflkasysmiqjxbunezgwcdpv\nohvflkatylmiqjxbpnozgwcdpr\nohvflkgtysziqjxbunezgwgdpr\nohvfljaiysmiqjxbuvezgwcdpr\nohvflkxtyslizjxbunezgwcdpr\nohzflkatysmiqjxbcnezgwcdar\nohvflkatysmiqjxbisecgwcdpr\nshvflkatyjmiqjkbunezgwcdpr\nmhvflkatysmiqjxvunezgwcdpk\nohfflkatysmiqjxbunczgwcppr\nohvflkatysmiqjkzunezgwcdpc\nohvflkatysmifjxbuneygwctpr\nohvflkatysmimjbbunezgwcdpe\nohvflkatjsciqjxbunezgwcdpa\nohvxlkatysmitjxbunezswcdpr\nohvslkatfsmiqjxbunezgwudpr\nohvflkatysmiqexbugezgwcdnr\nonvflkatysmiqjxkunezgtcdpr\nfhsflkalysmiqjxbunezgwcdpr\noyvflkatysmiqjobxnezgwcdpr\nohvflkatysmiqjxbunezswgdvr\nphvflkatyymiqjxvunezgwcdpr\noivflzutysmiqjxbunezgwcdpr\nohvflkftysmiqjxbunezkwcopr\nohvflkatysmwnjxbunezgwcdpp\nohvflkatysmiqkxcunezgwndpr\nphvklkatysmiqjhbunezgwcdpr\nohvflrawysmiqjxbunhzgwcdpr\nohvflkatysmiqjxbunecgwcdig\nohvflpakysmiqjxbunezgwrdpr\nodvflkatykmiqjxbunezglcdpr\nohtflkatysiiqjxblnezgwcdpr\nlhvfpkatysmiqjxbupezgwcdpr\nohvflkatdsmiqjpbunezgwcdps\nohvflkztysmiqjxvunezgwjdpr\nohvflbatysmxqoxbunezgwcdpr\nohvklkaigsmiqjxbunezgwcdpr\nohvfgkawysmiqjxbunezgwcdur\nohvflkatyskpqjlbunezgwcdpr\nohvflkatyqmiqjhbupezgwcdpr\nohqflkatysmiqjxzonezgwcdpr\nohxfnkatyymiqjxbunezgwcdpr\nohmflkatpsmiqjxbunezgwcdpw\nohvflkatysmiqjibnnewgwcdpr\nvevflkatysmiqjxbunezgwcypr\nohvflkatydmiqwxbungzgwcdpr\nohsrlkatysmiqjxbcnezgwcdpr\nohvflkptyvmiqexbunezgwcdpr\nopzflkatysmiqjxrunezgwcdpr\nohvflkitysmiqjxcunezgwcmpr\nohvflkatysmhhjxblnezgwcdpr\nohvflkatysfiqjxbunrzgwmdpr\nohvflkatyamibjxbunezgwcdpf\nohvflkalysmigjxbunezggcdpr\nohvflkatwsmisjxbunezgdcdpr\ndhvflkatysmlqjxbunszgwcdpr\nohvflkatysmiqjxbueeygwcbpr\nohvflkatgsmiqjnbunezhwcdpr\nsvvflkatysmiqjxbunezgwckpr\nopvflkatysmiqpxbufezgwcdpr\nohnvlkatysmiqjxbunezglcdpr\nphvflkutysjiqjxbunezgwcdpr\nohvflabtysmiqjjbunezgwcdpr\nouvflkatysmiqjsbunezgwcdpk\nosvflkatysmijjxbunezgwcypr\nowvflkatysmiqjxbukxzgwcdpr\nohvfliatvsmiljxbunezgwcdpr\nohvflkatysmiqjxbumezbwtdpr\nohvflkatyfcicjxbunezgwcdpr\nohvflkatysmiqldbunezgfcdpr\noqvflkatysmiqixkunezgwcdpr\nohvflkatysmiqjxbulezgicdpe\nohvflkatysmiqjxbuniegwcdpl\nohvflkatysmiqjwbunbzgwcdhr\nohvflkatysmiqjdbunezgwwdkr\nohqflkytysmiqjxbunezgwcdpc\nohvflkatysmigjxbunezqwwdpr\nohvfloatysmiqjpbumezgwcdpr\nohvklkathkmiqjxbunezgwcdpr\nohvflkstjsmiqjxbunezgwctpr\nohvvlkatysmiqjxbunewgwcdir\nohnflkatysmiqjxbunszgwcdlr\nohvflkatysmnqjxbunezgxcdlr\nohvfrkatysmiqjxbonezgwcdor\nihvflkatysmiqjxbuneogwcxpr\nohvflkatysmiqjxbunecgwcccr\nowvflkatysmivjxbunezgwjdpr\nohvflkgtysmiqjxbunczhwcdpr\nohyqlkatysmiqjxbunezgwcypr\nohvflkatysmiqjvbunezuwcdpw\nohvflkathsmiqmxbuoezgwcdpr\nehvjlkajysmiqjxbunezgwcdpr\nohvflkltysmiqjxblnezgwjdpr\noovflkvtfsmiqjxbunezgwcdpr\nolvfzkatysmiqjxyunezgwcdpr\nohvflkatysqitjxbunezgncdpr\nyhvflkatysmkqjxbunazgwcdpr\nzlvolkatysmiqjxbunezgwcdpr\nohvflpatysmiqjxbunezgwcapb\nohvflkatysmuqjxbunezgfcdur")

(defn part-1 [d]
  (let [freqs (map #(->> %
                         frequencies
                         vals
                         set)
                   (str/split-lines d))]
    (* (count (filter #(% 2) freqs))
       (count (filter #(% 3) freqs)))))

(defn charwise-and [a b]
  (->> (map vector a b)
       (filter (fn [[f s]] (= f s)))
       (map first)
       (apply str)))

(defn part-2 [d]
  (first (for [x (str/split-lines d)
               y (str/split-lines d)
               :let [res (charwise-and x y)]
               :when (= (count res)
                        (- (count x) 1))]
           res)))

(part-1 data)
(part-2 data)