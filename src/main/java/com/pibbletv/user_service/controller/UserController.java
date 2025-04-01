package com.pibbletv.user_service.controller;

import com.pibbletv.user_service.business.interfaces.UserService;
import com.pibbletv.user_service.domain.User;
import lombok.AllArgsConstructor;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;

import java.util.Map;

@RestController
@RequestMapping("/user")
@AllArgsConstructor
public class UserController {

    private final UserService userService;

    @PostMapping(value = "/saveUser")
    public Mono<Void> saveUser(@RequestBody Mono<Map<String, Object>> dataMono) {
        return dataMono.flatMap(data -> {
            String userId = (String) data.get("userId");
            String username = (String) data.get("username");
            return userService.saveUser(userId, username);
        });
    }

    @GetMapping(value = "/getUser")
    public Mono<User> getUser(@RequestParam String userId) {
        return userService.getUser(userId);
    }
}