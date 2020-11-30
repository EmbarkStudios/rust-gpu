struct Ray {
    vec3 origin;

    // Needs to be normalized
    vec3 direction;
};

struct RaySdfIntersectResult {
    float t;
    vec3 position;
};

#define RM_HIT           0
#define RM_OUTSIDE_FRONT 1
#define RM_OUTSIDE_BACK  2

struct RaySdfMarchParams {
    int MAX_STEPS;
    float STEP_CONSTANT;
};

RaySdfMarchParams ray_sdf_march_params() { return RaySdfMarchParams(70, 1.0); }

void ignore(in int) { }

uint ray_sdf_march(in Ray ray,
                   inout float t, // distace ray has travelled
                   in float min_t,
                   in float max_t,
                   out int num_steps,
                   in RaySdfMarchParams params) {
    // Keep track of where in the marching we were the closest to a surface
    float closest_t = min_t;
    float closest_closeness = 1e30;

    for (num_steps = 0; num_steps < params.MAX_STEPS; num_steps++) {
        float d = _scene(ray.origin + ray.direction * t);

        // One can raise this value to gain some performance by sacrificing some
        // quality.
        if (abs(d) < 0.001 * t) {
            return RM_HIT;
        } else {
            if (t > 0.0) {
                // closeness is defined in viewing angle, hence division by t
                float closeness = d / t;
                if (closeness < closest_closeness) {
                    closest_t = t;
                    closest_closeness = closeness;
                }
            }

            t += d * params.STEP_CONSTANT;

            if (t < min_t) {
                return RM_OUTSIDE_FRONT;
            } else if (t > max_t) {
                return RM_OUTSIDE_BACK;
            }
        }
    }

    // If we reached MAX_STEPS we probably traced pretty close
    // to a surface, so return whatever point was the closest:
    t = closest_t;
    return RM_HIT;
}

bool ray_sdf_intersect(in Ray ray,
                       in float min_t,
                       in float max_t,
                       out RaySdfIntersectResult result) {
    float t = min_t;

    int num_steps;
    if (ray_sdf_march(ray, t, min_t, max_t, num_steps, ray_sdf_march_params()) != RM_HIT) {
        if (t < min_t) {
            t = min_t;
        } else {
            return false;
        }
    }
    ignore(num_steps); // Can be useful to visualize, but otherwise unused

    vec3 position = ray.origin + ray.direction * t;
    result = RaySdfIntersectResult(t, position);
    return true;
}

vec3 sdf_gradient_at(in vec3 p) // for function _scene(p)
{
    const float h = 0.0001;
    const vec2 k = vec2(1, -1);
    return (k.xyy * _scene(p + k.xyy * h) + k.yyx * _scene(p + k.yyx * h) +
                     k.yxy * _scene(p + k.yxy * h) + k.xxx * _scene(p + k.xxx * h)) * 0.25;
}

vec3 sdf_diffuse_color_at(in vec3 position) { return _scene_color(position).rgb; }
