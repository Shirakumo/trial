#section VERTEX_SHADER
#include (trial:trial trial::standard-environment-information)
out vec3 v_position;
out vec3 v_normal;
out vec2 v_uv;

#section FRAGMENT_SHADER
#include (trial:trial trial::standard-light-block)
#include (trial:trial trial::standard-material-block)
#include (trial:trial trial::standard-environment-information)
in vec3 v_position;
in vec3 v_normal;
in vec3 v_uv;
layout (location = 0) out vec4 f_color;
layout (location = 1) out vec3 f_normal;
