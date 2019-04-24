#version 150 core

in vec2 position;
in vec4 color;

uniform mat4 projection;
uniform mat4 model;

out vec4 vertexColor;

void main()
{
  vertexColor = color;
  gl_Position = projection * model * vec4(position, 0.0, 1.0);
}