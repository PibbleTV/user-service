package com.pibbletv.user_service.business.implementations;

import com.pibbletv.user_service.persistance.repository.UserRepository;
import com.pibbletv.user_service.business.interfaces.UserService;
import com.pibbletv.user_service.persistance.entities.UserEntity;
import com.pibbletv.user_service.business.converters.UserConverter;
import com.pibbletv.user_service.domain.User;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.io.InputStream;

@Service
@AllArgsConstructor
public class UserServiceImpl implements UserService {

    private final UserRepository userRepository;

    @Override
    public Mono<Void> saveUser(String userId, String username) {

        byte[] defaultBgImage = loadDefaultImage("images/default-bg.webp");
        byte[] defaultProfileImage = loadDefaultImage("images/default-pfp.webp");

        return userRepository.findById(Long.parseLong(userId))
                .flatMap(existingUser -> Mono.empty())
                .switchIfEmpty(Mono.defer(() -> {

                    UserEntity userEntity = new UserEntity();
                    userEntity.setId(Long.parseLong(userId));
                    userEntity.setUsername(username);
                    userEntity.setBgImage(defaultBgImage);
                    userEntity.setProfileImage(defaultProfileImage);
                    userEntity.setIsBanned(false);
                    return userRepository.save(userEntity).then();
                }))
                .then();
    }

    @Override
    public Mono<User> getUser(String userId) {
        return userRepository.findById(Long.parseLong(userId))
                .map(UserConverter::convertToObject)
                .switchIfEmpty(Mono.error(new RuntimeException("User not found")));
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