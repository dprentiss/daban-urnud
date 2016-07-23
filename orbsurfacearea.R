ro <- 0.5  #orb radius
d <- sqrt(2) * ro  # orthogonal distance of orb centroid from axis of rotation
rw <- d # radius of water surface to axis of rotation, decision variable
n <- 1000000  # sample size

# generate uniform random cartesian points on the cylinder swept by water surface
# let x be the direction of the cylindrical axis

# generate uniform cylindrical coordinates for fixed radius rw
x <- ro * runif(n)  # uniform x coorditates, [0, ro]
theta <- pi * runif(n) - pi/2  # uniform azmuthal angles -pi/2 to pi/2, [0, pi]

# convert cylindrical to cartesian coordinates
y <- rw*cos(theta)
z <- rw*sin(theta)

cylPoints <- data.frame(x, y, z)
surfPoints <- cylPoints[which(x^2 + y^2 + (z + ro)^2 <= ro^2),]
pointRatio = nrow(surfPoints)/nrow(cylPoints)

cylArea = 2*ro*2*pi*rw
surfArea = pointRatio * cylArea