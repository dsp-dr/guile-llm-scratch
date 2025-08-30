;;; training-texts.scm --- Sample training data for LLM
;;; Classic texts from Project Gutenberg for training

(define-module (data training-texts)
  #:export (shakespeare-samples
            philosophy-samples
            literature-samples
            combined-training-corpus
            get-training-batch))

;;; Shakespeare samples (from various plays)
(define shakespeare-samples
  '("To be, or not to be, that is the question: Whether 'tis nobler in the mind to suffer The slings and arrows of outrageous fortune, Or to take arms against a sea of troubles And by opposing end them."
    "All the world's a stage, And all the men and women merely players; They have their exits and their entrances, And one man in his time plays many parts."
    "What's in a name? That which we call a rose By any other name would smell as sweet."
    "This above all: to thine own self be true, And it must follow, as the night the day, Thou canst not then be false to any man."
    "The course of true love never did run smooth."
    "Lord, what fools these mortals be!"
    "If music be the food of love, play on, Give me excess of it; that surfeiting, The appetite may sicken, and so die."
    "We are such stuff as dreams are made on, and our little life is rounded with a sleep."
    "Cowards die many times before their deaths; The valiant never taste of death but once."
    "The fault, dear Brutus, is not in our stars, but in ourselves."))

;;; Philosophy and wisdom literature
(define philosophy-samples
  '("The unexamined life is not worth living."
    "I think, therefore I am."
    "Man is condemned to be free; because once thrown into the world, he is responsible for everything he does."
    "The only true wisdom is in knowing you know nothing."
    "Life must be understood backward. But it must be lived forward."
    "God is dead. God remains dead. And we have killed him."
    "One cannot step twice in the same river."
    "The life of man is solitary, poor, nasty, brutish, and short."
    "I have learned to seek my happiness by limiting my desires, rather than in attempting to satisfy them."
    "The greatest happiness of the greatest number is the foundation of morals and legislation."))

;;; Classic literature excerpts
(define literature-samples
  '("It was the best of times, it was the worst of times, it was the age of wisdom, it was the age of foolishness, it was the epoch of belief, it was the epoch of incredulity."
    "Call me Ishmael. Some years ago—never mind how long precisely—having little or no money in my purse, and nothing particular to interest me on shore, I thought I would sail about a little and see the watery part of the world."
    "It is a truth universally acknowledged, that a single man in possession of a good fortune, must be in want of a wife."
    "Happy families are all alike; every unhappy family is unhappy in its own way."
    "In a hole in the ground there lived a hobbit. Not a nasty, dirty, wet hole, filled with the ends of worms and an oozy smell, nor yet a dry, bare, sandy hole with nothing in it to sit down on or to eat: it was a hobbit-hole, and that means comfort."
    "All children, except one, grow up. They soon know that they will grow up, and the way Wendy knew was this. One day when she was two years old she was playing in a garden, and she plucked another flower and ran with it to her mother."
    "The past is a foreign country; they do things differently there."
    "It was a bright cold day in April, and the clocks were striking thirteen."
    "I am an invisible man. No, I am not a spook like those who haunted Edgar Allan Poe; nor am I one of your Hollywood-movie ectoplasms."
    "If you really want to hear about it, the first thing you'll probably want to know is where I was born, and what my lousy childhood was like, and how my parents were occupied and all before they had me, and all that David Copperfield kind of crap, but I don't feel like going into it, if you want to know the truth."))

;;; Technical and scientific texts
(define technical-samples
  '("In the beginning was the Word, and the Word was with God, and the Word was God."
    "The Tao that can be spoken is not the eternal Tao. The name that can be named is not the eternal name."
    "Energy equals mass times the speed of light squared."
    "Natural selection is the differential survival and reproduction of individuals due to differences in phenotype."
    "The entropy of an isolated system never decreases."
    "For every action, there is an equal and opposite reaction."
    "The whole is greater than the sum of its parts."
    "Form follows function."
    "Less is more."
    "God does not play dice with the universe."))

;;; Combine all samples into training corpus
(define (combined-training-corpus)
  "Return combined training corpus from all sources"
  (append shakespeare-samples
          philosophy-samples
          literature-samples
          technical-samples))

;;; Get a random batch for training
(define (get-training-batch size)
  "Get a random batch of training samples"
  (let ((corpus (combined-training-corpus))
        (batch '()))
    (do ((i 0 (+ i 1)))
        ((>= i size) batch)
      (set! batch (cons (list-ref corpus 
                                  (random (length corpus)))
                        batch)))))

;;; Additional larger text samples
(define zen-motorcycle-excerpt
  "The Buddha, the Godhead, resides quite as comfortably in the circuits of a digital computer or the gears of a cycle transmission as he does at the top of a mountain or in the petals of a flower. To think otherwise is to demean the Buddha—which is to demean oneself. That is what I want to talk about in this Chautauqua.")

(define godel-escher-bach-excerpt  
  "The Tortoise and Achilles happen to be strolling through a garden. 'I have a simple test to see if you really understand Gödel's Theorem,' announced the Tortoise. 'What is it?' asked Achilles eagerly. 'I will tell you two statements,' said the Tortoise. 'One of them is a truth about numbers, and the other is Gödel's Theorem. You must tell me which is which.'")

(define mythical-man-month-excerpt
  "The bearing of a child takes nine months, no matter how many women are assigned. Many software tasks have this characteristic because of the sequential nature of debugging.")

;;; training-texts.scm ends here