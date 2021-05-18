library(here)
library(tidyverse)
library(gganimate)
library(animation)
library(emojifont)
library(ggimage)
library(extrafont)
library(extrafontdb)

font_import()
# loadfonts()

entries <- c("Tulip", "Orchid", "Rose")
winner <- sample(entries, 1)

# Define animation parameters
## Number of particles
n <- 50
## Number of time steps
times_steps <- 50
## Particle starting position in x axis
xstart <- runif(n, max = 1)
## Particle starting position in y axis
ystart <- runif(n, max = 1.1)
## Particle size
size <- runif(n, min = 2, max = 25)
## Define sideways movement
sideway_jitter <- seq(-0.05, 0.05, length.out = 100)
## Define gravity
gravity <- runif(n, min = 0.005, max = 0.025)
# create storage vectors
xpos <- rep(NA, n * times_steps)
ypos <- rep(NA, n * times_steps)

# loop through simulations
for(i in seq(times_steps)){
  if(i == 1){
    # initiate values
    xpos[1:n] <- xstart
    ypos[1:n] <- ystart
  } else {
    # specify datapoints to update
    first_obs <- (n*i - n + 1)
    last_obs <- (n*i)
    # update x position
    # random shift
    xpos[first_obs:last_obs] <- xpos[(first_obs-n):(last_obs-n)] - sample(sideway_jitter, n, TRUE)
    # update y position
    # lower by gravity
    ypos[first_obs:last_obs] <- ypos[(first_obs-n):(last_obs-n)] - gravity
    # reset if passed bottom screen
    xpos <- ifelse(ypos < -0.1, runif(n), xpos) # restart at random x
    ypos <- ifelse(ypos < -0.1, 1.1, ypos) # restart just above top
  }
}

new_labels <- fontawesome(c('fa-github', 'fa-twitter', "fa-laptop", "fa-coffee"))

# store in dataframe
flakes <- cbind.data.frame(x = xpos,
                               y = ypos,
                               s = size,
                               t = rep(1:times_steps, each = n),
                               labels = sample(new_labels, 2500, replace=T))

# Create plot
winner_ani <- flakes %>%
  ggplot(aes(x, y, size = s, frame = t)) +
  geom_text(family='fontawesome-webfont', size=14, aes(label = labels)) +
  scale_size_identity() +
  coord_cartesian(c(0, 1), c(0, 1.5)) +
  labs(title = "codeRtsv lucky draw",
       subtitle = paste0("Meeting on ", date()),
       caption = "CC BY-SA @cexynature") +
  theme_void() +
  # theme(panel.background = element_rect("white"))
  annotate("text", x = 0.5, y = 1.5, label = "Congratulations", size = 12) +
  annotate("text", x = 0.5, y = 1.3, label = paste(winner, "!!!!"), size = 15)


# Create animation
ani_plot <- winner_ani + transition_states(t,
                                     transition_length = 2,
                                     state_length = 1)

# Render animation
# ani_plot
anim_save("winner.gif", ani_plot)
