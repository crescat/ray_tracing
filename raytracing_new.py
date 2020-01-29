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


    def bias(self):
        self.orig += 0.001 * self.dir


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
                self.pixels[self.height - y - 1][x] = fun(x, y)


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
        #pixel += self.render_specular(hit, ray)

        # kr = fresnel(ray.dir, hit.norm, hit.obj.material.ior)
        # if kr < 1 and hit.obj.material.transmission > 0:
        #     pixel += self.render_refraction(hit, ray, depth) * (1 - kr)
        # if hit.obj.material.transmission < 1:
        #     pixel += self.render_reflection(hit, ray, depth) * kr
        return pixel


    def render_diffuse(self, hit):
        intensity = self.ambient_light.copy()
        for light in self.lights:
            shadow_ray = Ray(hit.point, light.point-hit.point)
            if self.closest_hit(shadow_ray) is not None:
                continue
            intensity += max(0, np.dot(shadow_ray.dir, hit.norm))
        return hit.obj.material.color * intensity.clip(0, 1) * (1-hit.obj.material.transmission)


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
        refl_ray.bias()
        return self.render_ray(refl_ray, depth+1) * hit.obj.material.reflexivity


    def render_refraction(self, hit, eye_ray, depth):
        refr_dir = refraction(eye_ray.dir, hit.norm, hit.obj.material.ior)

        if refr_dir is not None:
            refr_ray = Ray(hit.point, refr_dir)
            refr_ray.bias()
            return self.render_ray(refr_ray, depth+1)*hit.obj.material.transmission
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


class Material:
    def __init__(self, color, smoothiness, reflexivity, ior, transmission):
        self.color = color
        self.smoothiness = smoothiness
        self.reflexivity = reflexivity
        self.ior = ior
        self.transmission = transmission


class Sphere:
    def __init__(self, center, radius, material):
        self.center = center
        self.radius = radius
        self.radius2 = radius * radius
        self.material = material

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
            return HitPoint(pHit, pNorm, self)
        else:
            pHit = ray.orig + ray.dir * t0
            pNorm = normalize(pHit - self.center)
            return HitPoint(pHit, pNorm, self)


class Triangle:
    def __init__(self, p0, p1, p2, is_double_sided, material):
        self.p0 = p0
        self.p1 = p1
        self.p2 = p2
        self.is_double_sided = is_double_sided
        self.material = material


    def intersect(self, ray):
        # simple ver
        p0, p1, p2 = self.p0, self.p1, self.p2
        p0_p1 = p1 - p0
        p0_p2 = p2 - p0
        n = normalize(np.cross(p0_p1, p0_p2))

        n_dot_ray = np.dot(n, ray.dir)

        if n_dot_ray > 0 and not self.is_double_sided:
            return None
        if abs(n_dot_ray) < 0.0001:
            return None

        d = -np.dot(n, p0)

        t = -(np.dot(n, ray.orig) + d) / n_dot_ray

        if t < 0:
            return None

        p = ray.orig + t * ray.dir

        edge0 = p1 - p0
        p_p0 = p - p0
        c = np.cross(edge0, p_p0)
        if np.dot(n, c) < 0:
            return None

        edge1 = p2 - p1
        p_p1 = p - p1
        c = np.cross(edge1, p_p1)
        if np.dot(n, c) < 0:
            return None

        edge2 = p0 - p2
        p_p2 = p - p2
        c = np.cross(edge2, p_p2)
        if np.dot(n, c) < 0:
            return None

        return HitPoint(p, n, self)


    def intersect1(self, ray):
        # oller-Trunbore algorithm
        p0, p1, p2 = self.p0, self.p1, self.p2
        p0_p1 = p1 - p0
        p0_p2 = p2 - p0
        pvec = np.cross(ray.dir, p0_p2)
        det = np.dot(p0_p1, pvec)

        if abs(det) < 0.0001:
            return None

        inv_det = 1 / det
        tvec = ray.orig - p0
        u = np.dot(tvec, pvec) * inv_det
        if u < 0 or u > 1:
            return None

        qvec = np.cross(tvec, p0_p1)
        v = np.dot(ray.dir, qvec) * inv_det
        if v < 0 or v + u > 1:
            return None

        t = np.dot(p0_p2, qvec) * inv_det
        if t <= 0:
            return None
        hit_point = ray.orig + t * ray.dir
        hit_normal = normalize(np.cross(p0_p1, p0_p2))
        return HitPoint(hit_point, hit_normal, self)


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


def fresnel(incident, normal, ior):
    cosi = np.dot(incident, normal).clip(-1, 1)
    etai, etat = 1, ior
    if cosi > 0:
        etai, etat = etat, etai
    sint = etai / etat * (max(0, 1 - cosi * cosi) ** 0.5)
    if sint >= 1:
        return 1
    cost = max(0, 1 - sint * sint)** 0.5
    cosi = abs(cosi)
    rs = ((etat * cosi) - (etai * cost)) / ((etat * cosi) + (etai * cost))
    rp = ((etai * cosi) - (etat * cost)) / ((etai * cosi) + (etat * cost))
    return (rs * rs + rp * rp) / 2


def round_to_int(number):
    return int(round(number))


def main():
    canvas = Canvas(400, 300)
    screen = Screen(400, 300)
    scene = Scene(screen)

    material1 = Material(color = np.array([1,0,0]),
                         smoothiness = 80,
                         reflexivity = 0.8,
                         ior = 2.0,
                         transmission = 0)

    material2 = Material(color = np.array([0,1,0]),
                         smoothiness = 80,
                         reflexivity = 0.8,
                         ior = 1.5,
                         transmission = 0.95)

    material3 = Material(color = np.array([0,0,1]),
                         smoothiness = 80,
                         reflexivity = 0.8,
                         ior = 1.5,
                         transmission = 0)

    # scene.add_object(Sphere(center = np.array([0,-40,-550]),
    #                         radius = 100,
    #                         material = material1)

    # scene.add_object(Sphere(center = np.array([-80,30,-400],
    #                         radius = 50,
    #                         material = material2)

    scene.add_object(Triangle(p0 = np.array([0,-50,-800]),
                              p1 = np.array([80,20,-800]),
                              p2 = np.array([-60,30,-800]),
                              is_double_sided = False,
                              material = material3))

    #scene.add_light(Light(np.array([0,-300,-500]), np.array([0,0,1])))
    scene.add_light(Light(np.array([-200,400,-500])))

    canvas.for_each_pixel(scene.render_pixel)
    #canvas.save(f"canvas_new_{i:02}.png")
    canvas.save("canvas_new.png")

#for i in range(60):
#    t = i * 2 - 60
#    print(i, t)
#    main(t, i)

main()
