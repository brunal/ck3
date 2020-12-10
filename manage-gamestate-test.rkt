#lang racket

(require rackunit "manage-gamestate.rkt")

(define state-test1
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
       (instance (scale 0.9 0.9))))
     (ironman_manager (date date 867 1 1) (save_game . "") (save_storage . local))
     (date date 1170 3 22)
     (meta_main_portrait
      (type . female)
      (id . 101392)
      (age . 0.76)
      (genes
       (hair_color 16 246 12 250)
       (skin_color 102 148 101 142)
       (eye_color 36 246 64 244))))))

(check-equal? (get-element state-test1 'foo) #f)

(check-equal? (get-element state-test1 '(meta_data save_game_version)) 3)
(check-equal? (get-element (get-element state-test1 'meta_data)'save_game_version) 3)

(define ironman-manager (get-element state-test1 '(meta_data ironman_manager)))
(check-equal? ironman-manager
              '((date date 867 1 1) (save_game . "") (save_storage . local)))
(check-equal? (get-element ironman-manager 'save_storage) 'local)
(check-equal? (get-element ironman-manager '(save_storage)) 'local)
(check-equal? (get-element ironman-manager 'date) '(date 867 1 1))

(check-equal? (get-element state-test1 '(meta_data meta_main_portrait genes hair_color)) '(16 246 12 250))

(check-equal? (get-player-id state-test1) 101392)