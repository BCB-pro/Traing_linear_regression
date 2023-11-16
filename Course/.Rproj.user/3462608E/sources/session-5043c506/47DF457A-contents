library(ggplot2)
library(gganimate)
library(tibble)
library(tidyverse)

# Génération de données simulées
set.seed(1234)
X <- rnorm(100, mean = 175, sd = 10)
Res <- rnorm(100, mean = 0, sd = 5)
Y <- X + Res
data <- data.frame(X, Y)

# Calcul des droites de régression en dehors de ggplot
regression_lines <- tibble::tibble(
  frame = seq(0, 90, length.out = 100),
  intercept = mean(Y) - tan((90 - frame) * pi / 180) * mean(X),
  slope = tan((90 - frame) * pi / 180)
)



regression_lines$frame <- 90 - regression_lines$frame 


regression_lines <- regression_lines[50:100,]

# Création de l'animation
animation <- ggplot(data, aes(x = X, y = Y))+
  geom_point(size = 5, color = "#002e82")+
  transition_states(frame, transition_length = 2, state_length = 1)+
  enter_fade()+
  exit_fade()+
  geom_abline(data = regression_lines,
              aes(intercept = intercept, slope = slope),
              color = "#e23d18", size = 4
  )+
  theme_classic()+
  scale_x_continuous(limits = c(145,205), n.breaks = 10)+
  scale_y_continuous(limits = c(145,205), n.breaks = 10)+
  theme(axis.line = element_line(size = 3),
        axis.title = element_text(size = 25),
        axis.text = element_blank(),
        panel.background = element_rect(fill = "#fafafa"),
        plot.background = element_rect(fill = "#fafafa")
  )

# Enregistrement de l'animation en GIF
animate(
  animation,
  height = 500,
  width = 800,
  fps = 30,
  duration = 10,
  end_pause = 60,
  res = 100,
  renderer = gifski_renderer(),
  nframes = 100)


gganimate::anim_save("/home/baptiste.criniere/Documents/Training_linear_regression/Presentation/regression_animation.gif",
                     nframes = 100, duration = 5)



























































