import cv2 as cv

from cv_utils import util
from object_description import describe_segmented_image
from segmentation import detect_edges, \
    get_foreground_from_edges, get_object_markers_from_foreground, \
    segment_by_watershed, segment_foreground_meanshift


def load_image(path):
    return cv.imread(path)


def preprocess_image(img):
    # denoising
    denoised = cv.fastNlMeansDenoisingColored(img)
    # blurring and meanshift for edge enhancing and color reduction
    blur = cv.pyrMeanShiftFiltering(denoised, sp=10, sr=10)
    return blur


def analyze_image(im, pre_color_segmentation=False,
                  show_intermediate_steps=False,
                  save_intermediate_steps=False,
                  for_experiment="clevr"):
    dst = im.copy()
    if show_intermediate_steps:
        util.show_bgr_image(dst, "Original image")
    preprocessed = preprocess_image(im)
    if show_intermediate_steps:
        util.show_bgr_image(preprocessed, title="Denoised image")
    if save_intermediate_steps:
        util.save_bgr_image(preprocessed, title="Denoised image")

    edges = detect_edges(preprocessed)
    if show_intermediate_steps:
        util.show_gray_image(edges, title="Detected edges")
    if save_intermediate_steps:
        util.save_gray_image(edges, title="Detected edges")

    # find rough contours to detect forground
    foreground, background = get_foreground_from_edges(preprocessed, edges)

    if show_intermediate_steps:
        util.show_gray_image(background, title="Sure background (black)")
    if save_intermediate_steps:
        util.save_gray_image(background, title="Sure background (black)")

    if show_intermediate_steps:
        util.show_gray_image(foreground, title="Sure foreground")
    if save_intermediate_steps:
        util.save_gray_image(foreground, title="Sure foreground")

    if pre_color_segmentation:
        color_labels = segment_foreground_meanshift(foreground, preprocessed)
        if show_intermediate_steps:
            util.show_segmentation(color_labels, title="Color segmented foreground")
        if save_intermediate_steps:
            util.save_segmentation(color_labels, title="Color segmented foreground")
    else:
        color_labels = foreground

    local_maxima, markers = get_object_markers_from_foreground(foreground, color_labels, preprocessed,
                                                               show_intermediate_steps=show_intermediate_steps,
                                                               save_intermediate_steps=save_intermediate_steps)

    if show_intermediate_steps:
        util.show_segmentation(markers, title="Markers for Watershed segmentation")
    if save_intermediate_steps:
        util.save_segmentation(markers, title="Markers for Watershed segmentation")

    # Finding unknown region
    unknown = cv.subtract(background, local_maxima)

    # segment with watershed algorithm
    markers = segment_by_watershed(markers, preprocessed, unknown)
    markers = markers-1

    if show_intermediate_steps:
        util.show_segmentation(markers, title="Segmented image by Watershed algorithm")
    if save_intermediate_steps:
        util.save_segmentation(markers, title="Segmented image by Watershed algorithm")

    dst, data = describe_segmented_image(markers, dst, False)

    if show_intermediate_steps:
        util.show_bgr_image(dst, title="Annotated objects")
    if save_intermediate_steps:
        util.save_bgr_image(dst, title="Annotated objects")

    return dst, data
