ro <- 800  # orb radius
rw <- 1264
d <- sqrt(2) * ro  # orthogonal distance of orb centroid from axis of rotation
n <- 10000  # sample size

genPointSet <- function(ro, d, rw, n) {
  # generate uniform random cartesian points on the cylinder interval swept by water
  # let x be the direction of the cylindrical axis
  
  # generate uniform cylindrical coordinates for radius interval rw to 'bottom' of orb
  x <- ro * runif(n)  # uniform x coorditates, [0, ro]
  theta <- pi * runif(n) - pi/2  # uniform azmuthal angles, [-pi/2, pi/2]
  r <- (d + ro - rw) * runif(n) + rw  # uniform radii [rw, d + ro]

  # convert cylindrical to cartesian coordinates an populate data frame
  y <- r*cos(theta)
  z <- r*sin(theta)
  cylPoints <- data.frame(x, y, z)
  
  # test whether points are in the orb
  volPoints <- cylPoints[which(x^2 + y^2 + (z + d)^2 <= ro^2),]
  
  # calculate the ratio of points in the orb to points out of the orb
  pointRatio <- nrow(volPoints)/nrow(cylPoints)

  cylVol <- ((d + ro)^2 - rw^2) * pi * 2 * ro
  waterVol <- pointRatio * cylVol
  
  return(c(rw, waterVol, n))
}

print(genPointSet(ro, d, rw, n))

