#include "framebuffer.hpp"
#include "context.hpp"
#include "renderbuffer.hpp"
#include "texture.hpp"

#include "internal/wrapper.hpp"

#include "internal/modules.hpp"
#include "internal/tools.hpp"
#include "internal/glsl.hpp"
#include "internal/data_type.hpp"

#include <iostream>

void MGLFramebuffer_use_core(MGLFramebuffer * self) {
    const GLMethods & gl = self->context->gl;
    self->context->bind_framebuffer(self->framebuffer_obj);
    gl.Viewport(self->viewport[0], self->viewport[1], self->viewport[2], self->viewport[3]);
}

/* MGLContext.framebuffer(...)
 */
PyObject * MGLContext_meth_framebuffer(MGLContext * self, PyObject * const * args, Py_ssize_t nargs) {
    if (nargs != 2) {
        // TODO: error
        return 0;
    }

    PyObject * color_attachments = args[0];
    PyObject * depth_attachment = args[1];

    if (!PySequence_Check(color_attachments)) {
        PyObject * tuple = PyTuple_New(1);
        PyTuple_SET_ITEM(tuple, 0, color_attachments);
        color_attachments = tuple;
    } else {
        color_attachments = PySequence_Fast(color_attachments, "not iterable");
    }

    MGLFramebuffer * framebuffer = MGLContext_new_object(self, Framebuffer);

    const GLMethods & gl = self->gl;
    gl.GenFramebuffers(1, (GLuint *)&framebuffer->framebuffer_obj);

    if (!framebuffer->framebuffer_obj) {
        return 0;
    }

    self->bind_framebuffer(framebuffer->framebuffer_obj);

    int color_attachments_len = (int)PySequence_Fast_GET_SIZE(color_attachments);
    for (int i = 0; i < color_attachments_len; ++i) {
        PyObject * attachment = PySequence_Fast_GET_ITEM(color_attachments, i);
        if (attachment->ob_type == Renderbuffer_class) {
            MGLRenderbuffer * renderbuffer = SLOT(attachment, MGLRenderbuffer, Renderbuffer_class_mglo);
            gl.FramebufferRenderbuffer(
                GL_FRAMEBUFFER,
                GL_COLOR_ATTACHMENT0 + i,
                GL_RENDERBUFFER,
                renderbuffer->renderbuffer_obj
            );
        } else if (attachment->ob_type == Texture_class) {
            int level = PyLong_AsLong(SLOT(attachment, PyObject, Texture_class_level));
            MGLTexture * texture = SLOT(attachment, MGLTexture, Texture_class_mglo);
            gl.FramebufferTexture2D(
                GL_FRAMEBUFFER,
                GL_COLOR_ATTACHMENT0 + i,
                texture->samples ? GL_TEXTURE_2D_MULTISAMPLE : GL_TEXTURE_2D,
                texture->texture_obj,
                level
            );
        } else {
            return 0;
        }
    }

    Py_DECREF(color_attachments);

    int status = gl.CheckFramebufferStatus(GL_FRAMEBUFFER);
    self->bind_framebuffer(self->bound_framebuffer->framebuffer_obj);

    if (status != GL_FRAMEBUFFER_COMPLETE) {
        const char * message = "the framebuffer is not complete";

        switch (status) {
        	case GL_FRAMEBUFFER_UNDEFINED:
        		message = "the framebuffer is not complete (UNDEFINED)";
        		break;

        	case GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT:
        		message = "the framebuffer is not complete (INCOMPLETE_ATTACHMENT)";
        		break;

        	case GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT:
        		message = "the framebuffer is not complete (INCOMPLETE_MISSING_ATTACHMENT)";
        		break;

        	case GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER:
        		message = "the framebuffer is not complete (INCOMPLETE_DRAW_BUFFER)";
        		break;

        	case GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER:
        		message = "the framebuffer is not complete (INCOMPLETE_READ_BUFFER)";
        		break;

        	case GL_FRAMEBUFFER_UNSUPPORTED:
        		message = "the framebuffer is not complete (UNSUPPORTED)";
        		break;

        	case GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE:
        		message = "the framebuffer is not complete (INCOMPLETE_MULTISAMPLE)";
        		break;

        	case GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS:
        		message = "the framebuffer is not complete (INCOMPLETE_LAYER_TARGETS)";
        		break;
        }

        return 0;
    }

    framebuffer->viewport[0] = 0;
    framebuffer->viewport[1] = 0;
    framebuffer->viewport[2] = framebuffer->width;
    framebuffer->viewport[3] = framebuffer->height;
    SLOT(framebuffer->wrapper, PyObject, Framebuffer_class_viewport) = int_tuple(0, 0, framebuffer->width, framebuffer->height);
    return NEW_REF(framebuffer->wrapper);
}

/* MGLFramebuffer.readviewport=None, components=3, attachment=0, alignment=1, dtype='f1', np=False)
 */
PyObject * MGLFramebuffer_meth_read(MGLFramebuffer * self, PyObject * const * args, Py_ssize_t nargs) {
    std::cout << "Inside MGLFramebuffer_meth_read\n" << std::endl;
    if (nargs != 6) {
        // TODO: error
        std::cout << "Args not equal to 4\n" << nargs << std::endl;
        return 0;
    }


    std::cout << "Assigning args" << std::endl;
    PyObject * viewport = args[0];
    int components = PyLong_AsLong(args[1]);
    int attachment = PyLong_AsLong(args[2]);
    int alignment = PyLong_AsLong(args[3]);
    MGLDataType * data_type = from_dtype(args[4]);
    int np = PyObject_IsTrue(args[5]);
    std::cout << "Assigned args" << std::endl;

    int x = 0;
    int y = 0;
    int width = 0;
    int height = 0;

    if (!unpack_viewport(viewport, x, y, width, height)) {
        std::cout << "Inside If statement\n" << std::endl;
        return 0;
    }

    bool read_depth = attachment == -1;
    std::cout << "test 1" << std::endl;

    int expected_size = width * self->components * data_type->size;
    std::cout << "expected_size" << expected_size << std::endl;
    std::cout << "alignment" << alignment << std::endl;
    expected_size = (expected_size + alignment - 1) / alignment * alignment;
    std::cout << "test 3" << std::endl;
    expected_size = expected_size * height;

    std::cout << "test 2" << std::endl;

    int pixel_type = data_type->gl_type;
    int base_format = read_depth ? GL_DEPTH_COMPONENT : data_type->base_format[components];

    std::cout << "test 3" << std::endl;

    PyObject * result = PyBytes_FromStringAndSize(0, expected_size);
    char * data = PyBytes_AS_STRING(result);
    std::cout << data << std::endl;

    std::cout << "test 4" << std::endl;

    const GLMethods & gl = self->context->gl;

    self->context->set_alignment(alignment);
    self->context->bind_framebuffer(self->framebuffer_obj);
    gl.ReadBuffer(read_depth ? GL_NONE : (GL_COLOR_ATTACHMENT0 + attachment));
    gl.ReadPixels(x, y, width, height, base_format, pixel_type, data);
    self->context->bind_framebuffer(self->context->bound_framebuffer->framebuffer_obj);
    std::cout << "Before return\n" << std::endl;
    return result;
}

/* MGLFramebuffer.use()
 */
PyObject * MGLFramebuffer_meth_use(MGLFramebuffer * self) {
    MGLFramebuffer_use_core(self);
    Py_RETURN_NONE;
}

/* MGLFramebuffer.clear(attachment, value, viewport, color_mask)
 */
PyObject * MGLFramebuffer_meth_clear(MGLFramebuffer * self, PyObject * const * args, Py_ssize_t nargs) {
    if (nargs != 4) {
        // TODO: error
        return 0;
    }

    int attachment = PyLong_AsLong(args[0]);
    PyObject * value = args[1];
    PyObject * viewport = args[2];
    int color_mask = PyLong_AsLong(args[3]);

    char color_bytes[32] = {};
    bool scissor = false;

    const GLMethods & gl = self->context->gl;

    self->context->bind_framebuffer(self->framebuffer_obj);

    if (viewport != Py_None) {
        int x = 0;
        int y = 0;
        int width = 0;
        int height = 0;
        unpack_viewport(viewport, x, y, width, height);
        gl.Enable(GL_SCISSOR_TEST);
        gl.Scissor(x, y, width, height);
        scissor = true;
    } else if (self->viewport[0] || self->viewport[1] || self->viewport[2] != self->width || self->viewport[3] != self->height) {
        gl.Enable(GL_SCISSOR_TEST);
        gl.Scissor(self->viewport[0], self->viewport[1], self->viewport[2], self->viewport[3]);
        scissor = true;
    }

    if (attachment < 0) {
        float depth = (float)PyFloat_AsDouble(value);
        if (!self->context->current_depth_mask) {
            gl.DepthMask(true);
        }
        gl.ClearBufferfv(GL_DEPTH, 0, &depth);
        if (!self->context->current_depth_mask) {
            gl.DepthMask(false);
        }
    } else if (attachment < self->attachments) {
        value = PySequence_Fast(value, "value is not iterable");
        if (!value) {
            return 0;
        }
        read_value func;
        void * ptr = color_bytes;
        const char shape = self->attachment_type[attachment];
        switch (shape) {
            case 'f': func = (read_value)read_float; break;
            case 'i': func = (read_value)read_int; break;
            case 'u': func = (read_value)read_unsigned; break;
        }
        int size = (int)PySequence_Fast_GET_SIZE(value);
        for (int i = 0; i < size; ++i) {
            func(ptr, PySequence_Fast_GET_ITEM(value, i));
        }
        Py_DECREF(value);
        if (PyErr_Occurred()) {
            if (scissor) {
                gl.Disable(GL_SCISSOR_TEST);
            }
            return 0;
        }
        int old_mask = self->context->current_color_mask >> (attachment * 4) & 0xF;
        if (old_mask != color_mask) {
            gl.ColorMaski(attachment, color_mask & 1, color_mask & 2, color_mask & 4, color_mask & 8);
        }
        switch (shape) {
            case 'f': gl.ClearBufferfv(GL_COLOR, attachment, (float *)color_bytes); break;
            case 'i': gl.ClearBufferiv(GL_COLOR, attachment, (int *)color_bytes); break;
            case 'u': gl.ClearBufferuiv(GL_COLOR, attachment, (unsigned *)color_bytes); break;
        }
        if (old_mask != color_mask) {
            gl.ColorMaski(attachment, old_mask & 1, old_mask & 2, old_mask & 4, old_mask & 8);
        }
    } else {
        return 0;
    }

    if (scissor) {
        gl.Disable(GL_SCISSOR_TEST);
    }

    // return 0;
    Py_RETURN_NONE;
}

/* MGLFramebuffer.viewport
 */
int MGLFramebuffer_set_viewport(MGLFramebuffer * self, PyObject * value) {
    int x = 0;
    int y = 0;
    int width = self->width;
    int height = self->height;

    if (!unpack_viewport(value, x, y, width, height)) {
        return -1;
    }

    PyObject *& viewport_slot = SLOT(self->wrapper, PyObject, Framebuffer_class_viewport);
    // PyObject * viewport =
    // replace_object(viewport_slot, viewport);
    Py_XDECREF(viewport_slot);
    viewport_slot = int_tuple(x, y, width, height);
    self->viewport[0] = x;
    self->viewport[1] = y;
    self->viewport[2] = width;
    self->viewport[3] = height;
    if (self->context->bound_framebuffer == self) {
        self->context->gl.Viewport(x, y, width, height);
    }
    return 0;
}

PyObject * MGLContext_copy_framebuffer(MGLContext * self, PyObject * args) {
	PyObject * dst;
	MGLFramebuffer * src;

	int args_ok = PyArg_ParseTuple(
		args,
		"OO!",
		&dst,
		&Framebuffer_class,
		&src
	);

	if (!args_ok) {
		return 0;
	}

	const GLMethods & gl = self->gl;

	// If the sizes of the source and destination rectangles are not equal,
	// filter specifies the interpolation method that will be applied to resize the source image,
	// and must be GL_NEAREST or GL_LINEAR. GL_LINEAR is only a valid interpolation
	// method for the color buffer. If filter is not GL_NEAREST and mask includes
	// GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT, no data is transferred and a
	// GL_INVALID_OPERATION error is generated.

	if (Py_TYPE(dst) == Framebuffer_class) {

		// MGLFramebuffer * dst_framebuffer = (MGLFramebuffer *)dst;
        MGLFramebuffer * dst_framebuffer = MGLContext_new_object(self, Framebuffer);

		int width = 0;
		int height = 0;

		if (!dst_framebuffer->framebuffer_obj) {
			width = src->width;
			height = src->height;
		} else if (!src->framebuffer_obj) {
			width = dst_framebuffer->width;
			height = dst_framebuffer->height;
		} else {
			width = src->width < dst_framebuffer->width ? src->width : dst_framebuffer->width;
			height = src->height < dst_framebuffer->height ? src->height : dst_framebuffer->height;
		}

		gl.BindFramebuffer(GL_READ_FRAMEBUFFER, src->framebuffer_obj);
		gl.BindFramebuffer(GL_DRAW_FRAMEBUFFER, dst_framebuffer->framebuffer_obj);
		gl.BlitFramebuffer(
			0, 0, width, height,
			0, 0, width, height,
			GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT,
			GL_NEAREST
		);
		gl.BindFramebuffer(GL_FRAMEBUFFER, self->bound_framebuffer->framebuffer_obj);

	} else if (Py_TYPE(dst) == Texture_class) {

		MGLTexture * dst_texture = (MGLTexture *)dst;

		if (dst_texture->samples) {
            // handle the error here
            std::cout << "multisample texture targets are not accepted" << std::endl;
			return 0;
		}

		if (src->samples) {
            // handle the error here
			std::cout << "multisample framebuffer source with texture targets are not accepted" << std::endl;
			return 0;
		}

		int width = src->width < dst_texture->width ? src->width : dst_texture->width;
		int height = src->height < dst_texture->height ? src->height : dst_texture->height;

		if (!src->framebuffer_obj) {
			width = dst_texture->width;
			height = dst_texture->height;
		} else {
			width = src->width < dst_texture->width ? src->width : dst_texture->width;
			height = src->height < dst_texture->height ? src->height : dst_texture->height;
		}

		const int formats[] = {0, GL_RED, GL_RG, GL_RGB, GL_RGBA};
		int texture_target = dst_texture->samples ? GL_TEXTURE_2D_MULTISAMPLE : GL_TEXTURE_2D;
		int format = formats[dst_texture->components];

		gl.BindFramebuffer(GL_READ_FRAMEBUFFER, src->framebuffer_obj);
		gl.CopyTexImage2D(texture_target, 0, format, 0, 0, width, height, 0);
		gl.BindFramebuffer(GL_FRAMEBUFFER, self->bound_framebuffer->framebuffer_obj);

	} else {
        // handle the error here
        std::cout << "the dst must be a Framebuffer or Texture" << std::endl;
		return 0;

	}

	Py_RETURN_NONE;
}


#if PY_VERSION_HEX >= 0x03070000

PyMethodDef MGLFramebuffer_methods[] = {
    {"read", (PyCFunction)MGLFramebuffer_meth_read, METH_FASTCALL, 0},
    {"use", (PyCFunction)MGLFramebuffer_meth_use, METH_NOARGS, 0},
    {"clear", (PyCFunction)MGLFramebuffer_meth_clear, METH_FASTCALL, 0},
    {0},
};

#else

PyObject * MGLFramebuffer_meth_read_va(MGLFramebuffer * self, PyObject * args) {
    return MGLFramebuffer_meth_read(self, ((PyTupleObject *)args)->ob_item, ((PyVarObject *)args)->ob_size);
}

PyObject * MGLFramebuffer_meth_clear_va(MGLFramebuffer * self, PyObject * args) {
    return MGLFramebuffer_meth_clear(self, ((PyTupleObject *)args)->ob_item, ((PyVarObject *)args)->ob_size);
}

PyMethodDef MGLFramebuffer_methods[] = {
    {"read", (PyCFunction)MGLFramebuffer_meth_read_va, METH_VARARGS, 0},
    {"use", (PyCFunction)MGLFramebuffer_meth_use, METH_NOARGS, 0},
    {"clear", (PyCFunction)MGLFramebuffer_meth_clear_va, METH_VARARGS, 0},
    {0},
};

#endif

PyGetSetDef MGLFramebuffer_getset[] = {
    {"viewport", 0, (setter)MGLFramebuffer_set_viewport, 0, 0},
    {0},
};

PyType_Slot MGLFramebuffer_slots[] = {
    {Py_tp_methods, MGLFramebuffer_methods},
    {Py_tp_getset, MGLFramebuffer_getset},
    {0},
};

PyType_Spec MGLFramebuffer_spec = {
    mgl_name ".Framebuffer",
    sizeof(MGLFramebuffer),
    0,
    Py_TPFLAGS_DEFAULT,
    MGLFramebuffer_slots,
};