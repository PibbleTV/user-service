package com.pibbletv.user_service.business.implementations;

import com.pibbletv.user_service.business.ImageLoader;
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
    private final ImageLoader imageLoader;

    @Override
    public Mono<Void> saveUser(String userId, String username) {
        byte[] defaultBgImage = imageLoader.load("images/default-bg.webp");
        byte[] defaultProfileImage = imageLoader.load("images/default-pfp.webp");

        return userRepository.existsById(Long.parseLong(userId))
                .flatMap(exists -> {
                    if (exists) {
                        return Mono.error(new RuntimeException("User already exists"));
                    } else {
                        UserEntity userEntity = new UserEntity();
                        userEntity.setId(Long.parseLong(userId));
                        userEntity.setUsername(username);
                        userEntity.setBgImage(defaultBgImage);
                        userEntity.setProfileImage(defaultProfileImage);
                        userEntity.setIsBanned(false);
                        return userRepository.save(userEntity).then();
                    }
                });
    }

    @Override
    public Mono<User> getUser(String userId) {
        return userRepository.findById(Long.parseLong(userId))
                .map(UserConverter::convertToObject)
                .switchIfEmpty(Mono.error(new RuntimeException("User not found")));
    }

}