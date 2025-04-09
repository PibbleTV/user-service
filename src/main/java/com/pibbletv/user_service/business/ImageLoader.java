package com.pibbletv.user_service.business;

import org.springframework.stereotype.Component;

import java.io.IOException;
import java.io.InputStream;

@Component
public class ImageLoader {
    public byte[] load(String path) {
        try (InputStream inputStream = getClass().getClassLoader().getResourceAsStream(path)) {
            if (inputStream == null) {
                throw new RuntimeException("Image not found: " + path);
            }
            return inputStream.readAllBytes();
        } catch (IOException e) {
            throw new RuntimeException("Error reading image: " + path, e);
        }
    }
}
