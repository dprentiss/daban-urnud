ro <- 0.5  # orb radius
d <- sqrt(2) * ro  # orthogonal distance of orb centroid from axis of rotation
#rw <- d - .1  # radius of water surface to axis of rotation, decision variable
n <- 1000000  # sample size



genPointSet <- function(ro, d, rw, n) {
  # generate uniform random cartesian points on the cylinder swept by water surface
  # let x be the direction of the cylindrical axis
  
  # generate uniform cylindrical coordinates for fixed radius rw
  x <- ro * runif(n)  # uniform x coorditates, [0, ro]
  theta <- pi * runif(n) - pi/2  # uniform azmuthal angles -pi/2 to pi/2, [-pi/2, pi/2]

  # convert cylindrical to cartesian coordinates an populate data frame
  y <- rw*cos(theta)
  z <- rw*sin(theta)
  cylPoints <- data.frame(x, y, z)
  
  # test whether points are in the orb
  surfPoints <- cylPoints[which(x^2 + y^2 + (z + ro)^2 <= ro^2),]
  
  # calculate the ratio of points in the orb to points out of the orb
  pointRatio = nrow(surfPoints)/nrow(cylPoints)

  cylArea = 2*ro*2*pi*rw
  surfArea = pointRatio * cylArea
  
  return(c(rw, surfArea, n))
}

#for (r in seq(d - ro, d + ro, by=0.01)) {
#  print(genPointSet(ro, d, r, n))
#}

for (r in seq(0.59, 0.60, by=0.001)) {
  print(genPointSet(ro, d, r, n))
}
