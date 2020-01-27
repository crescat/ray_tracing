from PIL import Image
import numpy as np
import math
#Tabnine::no_sem
def BLACK():
    return np.array([0.0,0.0,0.0])

def WHITE():
    return np.array([1.0,1.0,1.0])
class Ray:
    def __init__(self, orig, dir):
        self.orig = orig
        self.dir = normalize(dir)

    def bias(self, hit_norm):
        self.orig += 0.01 * hit_norm


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

class Canvas:
    def __init__(self, w = 400, h = 300):
        self.width = w
        self.height = h
        self.pixels = [[BLACK() for _ in range(w)] for _ in range(h)]

    def for_each_pixel(self, fun):
        for x in range(self.width):
            for y in range(self.height):
                self.pixels[y][x] = fun(x, y)

    def save(self, output):
        pic = Image.new("RGB", (self.width, self.height))
        pixs = pic.load()
        for x in range(self.width):
            for y in range(self.height):
                [r,g,b] = np.round(self.pixels[y][x].clip(0, 1) * 255).tolist()
                pixs[x, y] = (int(r), int(g), int(b))
        pic.save(output)


class Scene:
    def __init__(self, screen):
        self.screen = screen
        self.objects = []
        self.lights = []
        self.ambient_light = np.array([0.4, 0.4, 0.4])
        self.default_color = BLACK()

    def add_object(self, obj):
        self.objects.append(obj)


    def add_light(self, light):
        self.lights.append(light)


    def closest_hit(self, ray):
        closest_hitpoint = None
        for obj in self.objects:
            hitPoint = obj.intersect(ray)
            min_distance = 9999999
            if hitPoint is not None:
                distance = np.linalg.norm(ray.orig - hitPoint.point)
                if min_distance > distance:
                    min_distance = distance
                    closest_hitpoint = hitPoint
        return closest_hitpoint


    def render_pixel(self, x, y):
        eye_ray = self.screen.generate_eye_ray(x, y)
        return self.render_ray(eye_ray, 0)


    def render_ray(self, ray, depth):
        if depth > 5:
            return BLACK()
        hit = self.closest_hit(ray)
        if hit is None:
            return self.default_color
        pixel = BLACK()

        pixel += self.render_diffuse(hit)
        pixel += self.render_specular(hit, ray)
        pixel += self.render_reflection(hit, ray, depth)
        pixel += self.render_refraction(hit, ray, depth)

        return pixel




    def render_diffuse(self, hit):
        intensity = self.ambient_light.copy()
        for light in self.lights:
            shadow_ray = Ray(hit.point, light.point-hit.point)
            if self.closest_hit(shadow_ray) is not None:
                continue
            intensity += max(0, np.dot(shadow_ray.dir, hit.norm))
        return hit.obj.color * intensity.clip(0, 1) * hit.obj.transmission


    def render_specular(self, hit, eye_ray):
        intensity = BLACK()
        for light in self.lights:
            shadow_ray = Ray(hit.point, light.point-hit.point)
            if self.closest_hit(shadow_ray) is not None:
                continue
            refl_ray = Ray(hit.point, reflection(shadow_ray.dir, hit.norm))
            glossy_angle = np.dot(eye_ray.dir, refl_ray.dir)
            light_intensity = light.color * light.brightness * max(0, glossy_angle) ** hit.obj.smoothiness
            intensity = np.maximum(intensity, light_intensity)
            intensity += light_intensity
        return intensity


    def render_reflection(self, hit, eye_ray, depth):
        refl_ray = Ray(hit.point, reflection(eye_ray.dir, hit.norm))
        refl_ray.bias(-hit.norm)
        return self.render_ray(refl_ray, depth+1) * hit.obj.reflexivity


    def render_refraction(self, hit, eye_ray, depth):
        refr_dir = refraction(eye_ray.dir, hit.norm, hit.obj.ior)

        if refr_dir is not None:
            refr_ray = Ray(hit.point, refr_dir)
            refr_ray.bias(-hit.norm)
            return self.render_ray(refr_ray, depth+1)*(1-hit.obj.transmission)
        return BLACK()

class Light:
    def __init__(self, point, color = WHITE(), brightness = 10):
        self.point = point
        self.brightness = brightness
        self.color = color

class HitPoint:
    def __init__(self, point, norm, obj):
        self.point = point
        self.norm = normalize(norm)
        self.obj = obj


class Sphere:
    def __init__(self, center, radius, color, smoothiness, reflexivity, ior, transmission):
        self.center = center
        self.radius = radius
        self.radius2 = radius * radius
        self.color = color
        self.smoothiness = smoothiness
        self.reflexivity = reflexivity
        self.ior = ior
        self.transmission = transmission

    def intersect(self, ray):
        L = self.center - ray.orig
        tca = np.dot(L, ray.dir)
        if tca < 0:
            return None
        d2 = np.dot(L, L) - tca * tca
        if d2 > self.radius2:
            return None
        thc = math.sqrt(self.radius2 - d2)
        t0 = tca - thc
        t1 = tca + thc
        if t0 < 0 and t1 < 0:
            return None
        elif t0 < 0:
            pHit = ray.orig + ray.dir * t1
            pNorm = normalize(pHit - self.center)
            return HitPoint(pHit, -pNorm, self)
        else:
            pHit = ray.orig + ray.dir * t0
            pNorm = normalize(pHit - self.center)
            return HitPoint(pHit, pNorm, self)


def normalize(vector):
    return vector / np.linalg.norm(vector)


def reflection(incident, normal):
    return incident - (2*np.dot(incident, normal)*normal)


def refraction(incident, normal, ior):
    cosi = np.dot(incident, normal).clip(-1, 1)
    etai, etat = 1, ior
    n = normal
    if cosi < 0:
        cosi = -cosi
    else:
        n = -normal
        etai, etat = etat, etai
    eta = etai / etat
    k = 1 - eta * eta * (1 - cosi * cosi)
    if k < 0:
        return None
    return (eta * incident + (eta * cosi - k ** 0.5) * n)


def round_to_int(number):
    return int(round(number))


def main(t):
    canvas = Canvas(400, 300)
    screen = Screen(400, 300)
    scene = Scene(screen)
    scene.add_object(Sphere(np.array([0,40,-550]), 100, np.array([1,0,0]), 80, 0.8, 2.0, 0.8))
    #scene.add_object(Sphere(np.array([10,-30,-600]), 100, np.array([0,0,1]), 80, 0.8, 1.5, 1))
    scene.add_object(Sphere(np.array([-80,-30,-400]), 50, np.array([0,1,0]), 80, 0.8, 1.5, 0))
    #scene.add_light(Light(np.array([0,-300,-500]), np.array([0,0,1])))
    scene.add_light(Light(np.array([-200,-400,-500])))

    canvas.for_each_pixel(scene.render_pixel)
    #canvas.save(f"canvas_new_{i:02}.png")
    canvas.save("canvas_new.png")

#for i in range(60):
#    t = i * 2 - 60
#    print(i, t)
#    main(t, i)

main(40)
