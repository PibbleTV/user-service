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
    public Mono<Void> addUser(String username) {
        byte[] defaultBgImage = imageLoader.load("images/default-bg.webp");
        byte[] defaultProfileImage = imageLoader.load("images/default-pfp.webp");

        return userRepository.findByUsername(username)
                .flatMap(user -> Mono.error(new RuntimeException("User already exists")))
                .switchIfEmpty(Mono.defer(() -> {
                    UserEntity userEntity = new UserEntity();
                    userEntity.setUsername(username);
                    userEntity.setBgImage(defaultBgImage);
                    userEntity.setProfileImage(defaultProfileImage);
                    userEntity.setIsBanned(false);
                    return userRepository.save(userEntity).then();
                }))
        .then();
    }

    @Override
    public Mono<Void> updateUser(User user) {
        if (user.getId() == null) {
            return Mono.error(new IllegalArgumentException("Cannot update user without ID"));
        }
        return userRepository.existsById(user.getId())
                .flatMap(exists -> {
                    if (!exists) {
                        return Mono.error(new IllegalArgumentException("User does not exist"));
                    }
                    return userRepository.save(UserConverter.convertToEntity(user)).then();
                });
    }

    @Override
    public Mono<Void> banUser(String username) {
        return userRepository.findByUsername(username)
                .switchIfEmpty(Mono.error(new IllegalArgumentException("User not found")))
                .flatMap(user -> {
                    if (Boolean.TRUE.equals(user.getIsBanned())) {
                        return Mono.error(new IllegalStateException("User is already banned"));
                    }
                    user.setIsBanned(true);
                    return userRepository.save(user);
                })
                .then();
    }

    @Override
    public Mono<Void> unbanUser(String username) {
        return userRepository.findByUsername(username)
                .switchIfEmpty(Mono.error(new IllegalArgumentException("User not found")))
                .flatMap(user -> {
                    if (Boolean.FALSE.equals(user.getIsBanned())) {
                        return Mono.error(new IllegalStateException("User is not banned"));
                    }
                    user.setIsBanned(false);
                    return userRepository.save(user);
                })
                .then();
    }

    @Override
    public Mono<User> getUser(String username) {
        return userRepository.findByUsername(username)
                .map(UserConverter::convertToObject)
                .switchIfEmpty(Mono.error(new RuntimeException("User not found")));
    }

}