package com.pibbletv.user_service.business.implementations;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import jakarta.annotation.PostConstruct;
import org.springframework.stereotype.Service;
import com.pibbletv.user_service.persistance.jpa.UserRepository;
import com.pibbletv.user_service.business.interfaces.UserService;
import com.pibbletv.user_service.persistance.entities.UserEntity;
import com.pibbletv.user_service.business.converters.UserConverter;
import com.pibbletv.user_service.domain.User;
import lombok.AllArgsConstructor;

import java.io.InputStream;
import java.security.KeyFactory;
import java.security.PublicKey;
import java.security.spec.X509EncodedKeySpec;
import java.util.Base64;

@Service
@AllArgsConstructor
public class UserServiceImpl implements UserService {

    private final UserRepository userRepository;


    @Override
    public void saveUser(String userId, String username) {

        if (userRepository.findById(Long.parseLong(userId)).isPresent()) {
            return;
        }

        byte[] defaultBgImage = loadDefaultImage("images/default-bg.webp");
        byte[] defaultProfileImage = loadDefaultImage("images/default-pfp.webp");

        UserEntity userEntity = UserEntity.builder()
                .id(userId)
                .username(username)
                .bgImage(defaultBgImage)
                .profileImage(defaultProfileImage)
                .isBanned(false)
                .build();

        userRepository.save(userEntity);
    }

    @Override
    public User getUser(String userId) {

        UserEntity userEntity = userRepository.findById(Long.parseLong(userId))
                .orElseThrow(() -> new RuntimeException("User not found with ID: " + userId));


        return UserConverter.convertToObject(userEntity);
    }


    private byte[] loadDefaultImage(String imagePath) {
        try (InputStream inputStream = getClass().getClassLoader().getResourceAsStream(imagePath)) {
            if (inputStream == null) {
                throw new RuntimeException("Default image not found: " + imagePath);
            }
            return inputStream.readAllBytes();
        } catch (Exception e) {
            throw new RuntimeException("Failed to load default image: " + imagePath, e);
        }
    }
}