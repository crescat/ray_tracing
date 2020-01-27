from PIL import Image
import numpy as np
import math

class Ray:
    def __init__(self, orig, dir):
        self.orig = orig
        self.dir = normalize(dir)



class Sphere:
    def __init__(self, center, radius, color):
        self.center = center
        self.radius = radius
        self.radius2 = radius * radius
        self.color = color

    def intersect(self, ray):
        L = np.subtract(self.center, ray.orig)
        tca = np.dot(L, ray.dir)
        if tca < 0:
            return (None, None)
        d2 = np.dot(L, L) - tca * tca
        if d2 > self.radius2:
            return (None, None)
        thc = math.sqrt(self.radius2 - d2)
        t0 = tca - thc
        t1 = tca + thc
        if t0 < 0 and t1 < 0:
            return (None, None)
        elif t0 < 0:
            pHit = ray.orig + ray.dir * t1
            pNorm = normalize(pHit - self.center)
            return (pHit, pNorm)
        else:
            pHit = ray.orig + ray.dir * t0
            pNorm = normalize(pHit - self.center)
            return (pHit, pNorm)


class Screen:
    def __init__(self, w = 400, h = 300, z_offset = -100):
        self.w = w
        self.h = h
        self.z_offset = z_offset
        self.size = (w, h)

    def generate_eye_ray(self, x, y, eye = np.array([0,0,500])):
        pixel_loc = self.get_pixel_location(x, y)
        return Ray(eye, pixel_loc - eye)

    def get_pixel_location(self, x, y):
        return np.array([x,y,0]) - np.array([self.w/2, self.h/2, -self.z_offset])



def normalize(vector):
    return vector / np.linalg.norm(vector)


def reflection(incident, normal):
    return incident - (2*np.dot(incident, normal)*normal)


def render(screen, obj_list, light_pos, output_path):
    canvas = Image.new("RGB", screen.size)
    (width, height) = screen.size
    pixels = canvas.load()
    for y in range(height):
        for x in range(width):
            minDistance = 9999999
            for obj in obj_list:
                eye_ray = screen.generate_eye_ray(x, y)
                (pHit, pNorm) = obj.intersect(eye_ray)
                if pHit is not None:
                    distance = np.linalg.norm(eye_ray.orig - pHit)
                    if minDistance > distance:
                        minDistance = distance
                        pixels[x, y] = obj.color

                    shadow_ray = Ray(pHit, light_pos - pHit)
                    for obj2 in obj_list:
                        (shadowHit, sNorm) = obj2.intersect(shadow_ray)
                        if shadowHit is None:
                            angle = np.dot(normalize(shadow_ray.dir), pNorm)
                            if angle < 0:
                                angle = 0
                            pixels[x, y] = np.array([int(round(x * (0.7 + 0.3*angle))) for x in obj.color])

                            refl_ray = Ray(pHit, reflection(shadow_ray.dir, pNorm))
                            glossy_angle = np.dot(normalize(eye_ray.dir), normalize(refl_ray.dir))
                            if glossy_angle < 0:
                                glossy_angle = 0
                            brightness = 255*(glossy_angle**10)




                        elif not np.array_equal(shadowHit, shadow_ray.orig):
                            pixels[x, y] = tuple([int(round(x * 0.7)) for x in obj.color])
                            break





    canvas.save(output_path)


sphere1 = Sphere(np.array([0,70,-500]), 100, (255,0,0))
sphere2 = Sphere(np.array([30,-110,-500]), 50, (0,255,0))
light_position = np.array([0,-300,-500])
obj_list = [sphere1, sphere2]
myscreen = Screen()
render(myscreen, obj_list, light_position, "canvas.png")
