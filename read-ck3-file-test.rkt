#lang racket

(require rackunit "read-ck3-file.rkt")


(define simple-save-string "meta_data={
        save_game_version=3
        version=\"1.2.1\"
        portraits_version=3
        meta_date=1170.3.22
        meta_player_name=\"Samrajni Amro la Dame du Gange\"
        meta_title_name=\"Empire Pallava\"
        meta_coat_of_arms={
                pattern=\"pattern_vertical_split_01.dds\"
                color1=yellow
                colored_emblem={
                        color1=orange
                        color2=orange
                        texture=\"ce_tiger_rampant.dds\"
                        instance={
                                scale={ 0.900000 0.900000 }
                        }
                }
        }
}
ironman_manager={
        date=867.1.1
        save_game=\"\"
        save_storage=local
}
date=1170.3.22
")
(define simple-save
  (read-ck3-file (open-input-string simple-save-string)))

(check-equal?
 simple-save
  '((meta_data
     (save_game_version . 3)
     (version . "1.2.1")
     (portraits_version . 3)
     (meta_date date 1170 3 22)
     (meta_player_name . "Samrajni Amro la Dame du Gange")
     (meta_title_name . "Empire Pallava")
     (meta_coat_of_arms
      (pattern . "pattern_vertical_split_01.dds")
      (color1 . yellow)
      (colored_emblem
       (color1 . orange)
       (color2 . orange)
       (texture . "ce_tiger_rampant.dds")
       (instance (scale 0.9 0.9)))))
    (ironman_manager (date date 867 1 1) (save_game . "") (save_storage . local))
    (date date 1170 3 22)))

(define list-of-structures-string "variables={
        data={ {
                        flag=\"scottish_culture_creation\"
                        data={
                                type=boolean
                                identity=1
                        }
                }
 {
                        flag=\"visigothic_culture_split\"
                        data={
                                type=boolean
                                identity=1
                        }
                }
}
}")
(define list-of-structures
  (read-ck3-file (open-input-string list-of-structures-string)))

(check-equal?
 list-of-structures
  '((variables
     (data
      ((flag . "scottish_culture_creation")
       (data (type . boolean) (identity . 1)))
      ((flag . "visigothic_culture_split")
       (data (type . boolean) (identity . 1)))))))

(define rgb-string "12029={
        key=\"x_mc_0\"
        name=\"Bande ashanti\"
        date=1103.1.1
        history={ 867.1.1={
                        type=created
                        holder=9429
                }
 899.10.14=9431 911.4.12=9433 912.3.5=36204 919.5.22=16787613 949.3.1=16818899 971.12.8=16829076 997.10.20=61757 1005.8.14=50384716 1036.8.28=16834664 1042.12.29=33626056 1056.1.6=67142732 1077.10.2=33629982 1087.5.4=16870951 1103.1.1={
                        type=destroyed
                        holder=16870951
                }
 }
        color=rgb { 255 0 81 }  landless=yes
        destroy_if_invalid_heir=yes
        no_automatic_claims=yes
        definite_form=yes
        coat_of_arms_id=2191
}")
(define rgb (read-ck3-file (open-input-string rgb-string)))
(check-equal?
 rgb
 '((12029
     (key . "x_mc_0")
     (name . "Bande ashanti")
     (date date 1103 1 1)
     (history
      ((date 867 1 1) (type . created) (holder . 9429))
      ((date 899 10 14) . 9431)
      ((date 911 4 12) . 9433)
      ((date 912 3 5) . 36204)
      ((date 919 5 22) . 16787613)
      ((date 949 3 1) . 16818899)
      ((date 971 12 8) . 16829076)
      ((date 997 10 20) . 61757)
      ((date 1005 8 14) . 50384716)
      ((date 1036 8 28) . 16834664)
      ((date 1042 12 29) . 33626056)
      ((date 1056 1 6) . 67142732)
      ((date 1077 10 2) . 33629982)
      ((date 1087 5 4) . 16870951)
      ((date 1103 1 1) (type . destroyed) (holder . 16870951)))
     (color rgb 255 0 81)
     (landless . yes)
     (destroy_if_invalid_heir . yes)
     (no_automatic_claims . yes)
     (definite_form . yes)
     (coat_of_arms_id . 2191))))


(define hybrid-structure-string "levels={ 10 0=1 1=2 }")
(define hybrid-structure (read-ck3-file (open-input-string hybrid-structure-string)))
(check-equal? hybrid-structure '((levels
   10
   (0 . 1)
   (1 . 2))))