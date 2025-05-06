package com.pibbletv.user_service.business.interfaces;

import com.pibbletv.user_service.domain.User;
import org.springframework.web.bind.annotation.RequestParam;
import reactor.core.publisher.Mono;

public interface UserService {
    Mono<Void> addUser(String username);
    Mono<Void> updateUser(User user);
    Mono<Void> banUser(String username);
    Mono<Void> unbanUser(String username);
    Mono<User> getUser(String username);
}